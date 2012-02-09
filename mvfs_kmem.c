/* * (C) Copyright IBM Corporation 1991, 2006. */
/*
 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; if not, write to the Free Software
 Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301, USA


 Author: IBM Corporation
 This module is part of the IBM (R) Rational (R) ClearCase (R)
 Multi-version file system (MVFS).
 For support, please visit http://www.ibm.com/software/support
*/

/* mvfs_kmem.c */
#include "mvfs_systm.h"
#include "mvfs.h"

#ifndef MVFS_SYSTEM_KMEM

#ifdef KMEMDEBUG
/*
 * Wrappers for kernel allocators to keep info
 * on who allocated what.
 */

#define MVFS_KMEMMAGIC	0xdebafa68
#define MVFS_KMEMFILL	0xdebeef69
#define MVFS_KMEM_FREEFILL	0xfeedface
struct mfs_kmemhead {
    struct  mfs_kmemhead *next;
    VOID_PTR block;
    u_long  thisblksize;
    caddr_t returnpc;
    caddr_t returnpc2;
    u_long  magic;
};

struct mfs_kmemhead *mfs_kmalloc = NULL;
u_long mfs_kmtotal = 0;	/* Total bytes allocated */
u_long mfs_kmcount = 0;	/* Total number of blocks allocated */
SPLOCK_T mfs_kmlock;
int mfs_kmparanoid = 0;	/* Causes validation of allocated list on every call */
int mfs_print_kmem = 0;		/* Flag to set to 'print the heap' */

#ifdef	MVFS_KMEM_ADDALLOC
#ifndef MVFS_KMRBUFFER_SZ
#define MVFS_KMRBUFFER_SZ	4096
#endif

struct _mvfs_kmem_rbuffer mvfs_kmem_rbuffer[MVFS_KMRBUFFER_SZ];
struct _mvfs_kmem_rbuffer *mvfs_kmem_rbuffer_curr = mvfs_kmem_rbuffer;
struct _mvfs_kmem_rbuffer *mvfs_kmem_rbuffer_end = &mvfs_kmem_rbuffer[MVFS_KMRBUFFER_SZ];
#endif

#endif

/*
 * The basic idea of the slab allocation stuff here is to take hunks
 * of memory from the system and manage them ourselves, so that we can
 * be quicker about getting memory for certain operations that happen
 * often.  We keep a free list, sorted by page.  If we have a totally
 * free page, it's returned to the system provided at least one other
 * page has at least one open slot left for the caller.
 *
 * At the moment, only the process and thread structures are allocated
 * from these private slabs.
 *
 * Do not attempt to use the slab allocator for large-ish items,
 * say bigger than 512 bytes.  It's really intended for items that will
 * easily fit many to a 4k page.
 *
 * Things to do:
 *
 * 	slide the start of the array of structures on each page, to
 * 	reduce cache footprint collisions (this is like assigning
 * 	colors to slabs as in the paper)
 *
 * Some of this stuff was inspired by the following paper; some of it
 * was already in our heads when we heard/read the paper:
 *
 * "The Slab Allocator: An Object-Caching Kernel Memory Allocator",
 * Jeff Bonwick, Sun Microsystems.  In Proceedings of the Summer 1994
 * USENIX Conference Proceedings, Boston, MA, pages 87--98.
 */

#define MVFS_SLABSIZE	4096		/* largest of the page sizes on
					   ports */
/*
 * This slab footer is placed at the end of every slab in a slab list.
 * It's used for bookkeeping and slab management within each individual
 * slab, as well as for slab specific free list management.
 */
struct mvfs_slab_footer {
    struct mvfs_slab_footer *slb_nextslab; /* chain to next slab */
    struct mvfs_slab_footer *slb_prevslab; /* chain to prev slab */
    struct mvfs_slab_freelist *slb_free; /* chain to free elt on this page */
    int slb_eltcount;			/* count of elts */
    int slb_freecount;			/* count of free elts */
    unsigned int slb_baseoffset;	/* offset from footer to base of stg */
    int filler;                         /* To align footer for 64 bit systems */
};
typedef struct mvfs_slab_footer mvfs_slab_footer_t;

/* This struct manages the slab list for each slab type. 
 * It has a lock for the list and keeps total counts of 
 * allocated and free elements in the entire list.
 */

struct mvfs_slab_list {
    SPLOCK_T mvfs_slablock;                 /* lock to protect this list */
    struct mvfs_slab_footer *slb_head;      /* head of list */
    unsigned int slb_eltsize;		    /* size of elements--used
					       to find things on this slab */
    unsigned int slb_eltpayload_size;       /* payload_size within each elem */

    int slb_tot_eltcount;                   /* total elements in list */
    int slb_tot_freecount;                  /* total free elements in list */
    short slb_traced;			    /* is it traced? */
};
typedef struct mvfs_slab_list mvfs_slab_list_t;
    
/*
 * We don't assume the slabs are 4k-aligned, although doing so would be a
 * great help---we could then just truncate pointers to find the slab control
 * for a given elt.
 *
 * Instead we put a single pointer before each allocated chunk of
 * returned data, and use that pointer to find the slab control.  Thus
 * the mvfs_slab_freelist structure overlays the (formerly) allocated
 * elt by one pointer.  [Every structure to be slab allocated must be
 * at least one pointer long to allow for the fre_next pointer to fit.]
 *
 * This scheme is kept hidden from the callers of the slab allocation routines 
 * and the callers are expected to pass in the actual size of the structure
 * they need.
 */

struct mvfs_slab_freelist {
    struct mvfs_slab_footer *fre_page;
    struct mvfs_slab_freelist *fre_next; /* NULL if last on page */
};

typedef union mvfs_aligner {
    struct mvfs_slab_freelist te_flist;
    struct {
	void *foo_spacer;
	union {
            /*
             * Put data types with most restrictive alignment requirements
             * into this union, to make sure returned storage is allocated
             * appropriately.
             */
	    MVFS_MAX_ALIGN_T foo_long;
	    MVFS_MAX_ALIGN_T *foo_ptr;
	} align;
    } foo;
} mvfs_aligner_t;

STATIC mvfs_slab_footer_t 
	*mvfs_new_slab_int(P1(unsigned int *size) PN(int traced));

STATIC void
mvfs_free_slab(
    struct mvfs_slab_footer *footer,
    short traced
);

#ifdef MVFS_KMEMTRACE
void mvfs_ktrace_init(P_NONE);
void mvfs_ktrace_unload(P_NONE);
#endif

STATIC int mvfs_kmem_init_count = 0;	/* some ports may need to do
					   early slab inits, so we count. */
STATIC int mvfs_kmem_extra_inits = 0;	/* some ports need extra early init,
					   and jump-start inside the first
					   alloc.  If extra_inits ==
					   init_count, then we can unload. */

#ifdef KMEMDEBUG
mvfs_slab_list_t * mvfs_kmemhead_slab; /* slab for kmem headers */
#endif

/*
 * Init routine always called, this is a noop if not
 * compiled with kmemdebug.
 */

void
mfs_kmem_init()
{
    if (mvfs_kmem_init_count++ > 0)
	return;				/* already done */
#ifdef KMEMDEBUG
    INITSPLOCK(mfs_kmlock,"mvfs_kmem_spl");
    mvfs_kmemhead_slab = mvfs_create_slablist(sizeof(struct mfs_kmemhead), FALSE, "mvfs_kmem_slab");
#endif
#ifdef MVFS_KMEMTRACE
    mvfs_ktrace_init();
#endif
}

/*
 * Free routine always called, this is a noop if not
 * compiled with kmemdebug.
 */

void
mfs_kmem_unload()
{
    if (--mvfs_kmem_init_count > mvfs_kmem_extra_inits)
	return;				/* not yet... */
#ifdef MVFS_KMEMTRACE
    mvfs_ktrace_unload();
#endif
#ifdef KMEMDEBUG
    {
	if (mfs_kmalloc != NULL) {
	    MVFS_PRINTF("mfs_kmem_unload: Found storage allocated, dumping kmem alloc list\n");
	    mfs_print_kmem = 1;
	    mfs_prkmem();
	}
	else {
	    MVFS_PRINTF("mfs_kmem_unload: No outstanding storage allocated\n");
	}

        mvfs_destroy_slablist(mvfs_kmemhead_slab);
	mvfs_kmemhead_slab = NULL;
    }
    FREESPLOCK(mfs_kmlock);
#endif
}

/*
 * MFS_PRHEAP - routine to print out (onto console) the
 * 		list of allocated blocks.  Always called
 *		from 'sync' code, but a noop if not compiled
 *		with kmemdebug.
 *
 * NOTE:  this routine effectively 'freezes' the MFS while
 * 	  it is printing its stuff!
 */

#define MVFS_PRKMLIST_SZ	512
void
mfs_prkmem()
{
#ifdef KMEMDEBUG
 
    typedef struct {
	struct mfs_kmemhead *ptr;
        caddr_t returnpc;
        caddr_t returnpc2;
	u_long size;
    } kmlist_t;

    struct mfs_kmemhead *p;
    kmlist_t *kmlist;
    int i, entcnt;
    SPL_T s;
    
    if (mfs_print_kmem != 0) {
	mfs_print_kmem = 0;	/* Clear flag in case parallel syncs */
	MVFS_PRINTF("---- mvfs alloc map:  time=0x%"MVFS_FMT_CTIME_X"  bytes=%d  blocks=%d ----\n", 
			MDKI_CTIME(), mfs_kmtotal, mfs_kmcount);
	kmlist = (kmlist_t *)REAL_KMEM_ALLOC(MVFS_PRKMLIST_SZ*sizeof(kmlist[0]), KM_NOSLEEP);
	if (kmlist == NULL) {
	    MVFS_PRINTF("No memory for print list\n");
	    return;
	}
	BZERO(kmlist, MVFS_PRKMLIST_SZ*sizeof(kmlist[0]));
        SPLOCK(mfs_kmlock, s);
        for (i=0, p = mfs_kmalloc; p != NULL && i < MVFS_PRKMLIST_SZ; p = p->next, i++) {
	    kmlist[i].ptr = p;
	    kmlist[i].returnpc = p->returnpc;
	    kmlist[i].returnpc2 = p->returnpc2;
	    kmlist[i].size = p->thisblksize;
	}
        entcnt = i;
	SPUNLOCK(mfs_kmlock, s);

	/* 
	 * Now, print stuff out without spinlock held. 
	 * Note that it is OK to print in multiples of 4 
	 * (past the last ent) since * the array allocated above 
	 * was allocated as a multiple of 4 ents, and the 
	 * contents were pre-zeroed.
	 */

	for (i=0; i < entcnt; i+=4) {
	    MVFS_PRINTF("%"KS_FMT_PTR_T" %"KS_FMT_PTR_T" %d\t%"KS_FMT_PTR_T" %"KS_FMT_PTR_T" %d\t%"KS_FMT_PTR_T" %"KS_FMT_PTR_T" %d\t%"KS_FMT_PTR_T" %"KS_FMT_PTR_T" %d\n", 
		kmlist[i].ptr,   kmlist[i].returnpc,   kmlist[i].size,
		kmlist[i+1].ptr, kmlist[i+1].returnpc, kmlist[i+1].size,
		kmlist[i+2].ptr, kmlist[i+2].returnpc, kmlist[i+2].size,
		kmlist[i+3].ptr, kmlist[i+3].returnpc, kmlist[i+3].size);
	    MDKI_USECDELAY(100000);
	}
	REAL_KMEM_FREE(kmlist, MVFS_PRKMLIST_SZ*sizeof(kmlist[0]));
    }
#endif
}

/*
 * Rest of routines only present when compiled with KMEMDEBUG
 */

#ifdef KMEMDEBUG
#define rndup(val,size) (((val) + (size) - 1) & ~((size)-1))

void
mfs_beparanoid(void);
VOID_PTR
mfs_kzalloc_common(
    size_t size,
    int flag,
    caddr_t ra,
    caddr_t rara
);

void
mfs_beparanoid(void)
{
    struct mfs_kmemhead *p;
    size_t asize;
    SPL_T s;

    /* If being paranoid, scan whole alloc list for integrity */

    SPLOCK(mfs_kmlock, s);
    for (p = mfs_kmalloc; p != NULL; p = p->next) {
        if (p->magic != MVFS_KMEMMAGIC) {
	    SPUNLOCK(mfs_kmlock, s);
	    MVFS_PRINTF("ptr = %"KS_FMT_PTR_T": ", p);
	    MDKI_PANIC("bad km magic\n");
	}
        asize = rndup(p->thisblksize, sizeof(MVFS_MAX_ALIGN_T));
        asize += sizeof(MVFS_MAX_ALIGN_T);
        if (*(MVFS_MAX_ALIGN_T *)((caddr_t)p->block + asize -
                                  sizeof(MVFS_MAX_ALIGN_T)) != MVFS_KMEMMAGIC)
        {
	    MVFS_PRINTF("overrun ptr=%lx kh=%lx: ", p->block, p);
	    MDKI_PANIC("bad km magic\n");
        }
    }
    SPUNLOCK(mfs_kmlock, s);

}

VOID_PTR
mfs_kzalloc_common(
    size_t size,
    int flag,
    caddr_t ra,	/* Return PC */
    caddr_t rara	/* Return PC */
)
{
    struct mfs_kmemhead *p;
    caddr_t rv;
    SPL_T s;
    register int i;
    size_t asize;

    if (mvfs_kmem_init_count == 0) {
	mvfs_kmem_extra_inits++;
	mfs_kmem_init();		/* need to do an extra init */
    }

    if (mfs_kmparanoid) mfs_beparanoid();

    /* round up asize to alignment size + 1 int */
    asize = rndup(size, sizeof(MVFS_MAX_ALIGN_T));
    asize += sizeof(MVFS_MAX_ALIGN_T);

    rv = REAL_KMEM_ALLOC(asize, flag);
    if (rv == NULL)
	return rv;			/* not available. */
    p = (struct mfs_kmemhead *) mvfs_slab_getchunk(mvfs_kmemhead_slab,
						   sizeof(struct mfs_kmemhead));
    if (p == NULL) {
        REAL_KMEM_FREE(rv, asize);
        return(p);
    }

    /* fill it with MVFS_KMEMFILL, then zero any unaligned slop */
    for (i = 0; i < asize; i += sizeof(int)) {
	*(int *)(rv + i) = MVFS_KMEMFILL;
    }
    if (i != 0 && i != asize)
	BZERO(rv + i - sizeof(int), size - i + sizeof(int));
    *(MVFS_MAX_ALIGN_T*)(rv + asize - sizeof(MVFS_MAX_ALIGN_T)) = MVFS_KMEMMAGIC;
    SPLOCK(mfs_kmlock, s);
    p->magic = MVFS_KMEMMAGIC;
    p->thisblksize = (u_long)size;
    p->returnpc = ra;
    p->returnpc2 = rara;
    p->block = rv;
    p->next = mfs_kmalloc;
    mfs_kmalloc = p;
    mfs_kmtotal += p->thisblksize;
    mfs_kmcount++;
    MVFS_KMEM_ADDALLOC(rv, ra, rara, size); /* Add to alloc ringbuffer */
    SPUNLOCK(mfs_kmlock, s);
    return rv;
}

VOID_PTR
mfs_kalloc(
    size_t size,
    int flag,
    caddr_t ra,	/* Return address */
    caddr_t rara /* Return address's return address */
)
{
    return(mfs_kzalloc_common(size, flag, ra, rara));
}

void
mfs_kfree(
    VOID_PTR ptr,
    size_t size,
    caddr_t ra,	/* Return address */
    caddr_t rara	/* Return address's return address */
)
{
    size_t asize;
    struct mfs_kmemhead *p;
    struct mfs_kmemhead *p0;
    SPL_T s;
    register int i;

    ASSERT(ptr != NULL);

    /* If being paranoid, scan whole alloc list for integrity */

    if (mfs_kmparanoid) mfs_beparanoid();

    /* Remove from allocated linked list */

    SPLOCK(mfs_kmlock, s);

    /* Quick check for first item on list */

    if (mfs_kmalloc->block == ptr) {
	p = mfs_kmalloc;
	mfs_kmalloc = p->next;
	goto checkitout;
    }

    /* Scan alloc list for this block */
   
    for (p0 = mfs_kmalloc; p0 != NULL; p0 = p0->next) {
	p = p0->next;
        if (p == NULL) break;
	if (p->block == ptr) {
	    p0->next = p->next;
	    goto checkitout;
	}
    }
   
    MVFS_PRINTF("mvfs: invalid kmfree ptr = 0x%"KS_FMT_PTR_T" mfs_kmalloc = 0x%"KS_FMT_PTR_T"\n", ptr,
	   mfs_kmalloc); 
    MDKI_PANIC("mvfs: bad kmem_free\n");

checkitout:
    /* Now add next free block */

    mfs_kmtotal -= p->thisblksize;
    mfs_kmcount--;
    SPUNLOCK(mfs_kmlock, s);
    if (p == NULL || p->magic != MVFS_KMEMMAGIC) {
	MVFS_PRINTF("mvfs: invalid kmfree magic number! hdr = 0x%"KS_FMT_PTR_T" ptr = 0x%"KS_FMT_PTR_T"\n",
	       p, ptr);
	MDKI_PANIC("mvfs: bad kmem_free");
    }
    if (p->thisblksize != size) {
	MVFS_PRINTF("mvfs: invalid kmfree size size=%d, sb, %d\n", size, p->thisblksize);
 	MDKI_PANIC("mvfs: bad kmem_free");
    }
    asize = rndup(size, sizeof(MVFS_MAX_ALIGN_T));
    asize += sizeof(MVFS_MAX_ALIGN_T);
    if (*(MVFS_MAX_ALIGN_T *)((caddr_t)ptr + asize -
                              sizeof(MVFS_MAX_ALIGN_T)) != MVFS_KMEMMAGIC)
    {
	MVFS_PRINTF("mvfs: kmfree of block overrunning its end, ptr=%d size=%ld asize=%ld\n", ptr, size, asize);
 	MDKI_PANIC("mvfs: bad kmem_free");
    }        
    /* fill freed memory */
    for (i = 0; i < asize / sizeof(u_int); i += sizeof(u_int)) {
        ((u_int *)ptr)[i] = MVFS_KMEM_FREEFILL;
    }
    REAL_KMEM_FREE(ptr, asize);
    mvfs_slab_freechunk(mvfs_kmemhead_slab, (caddr_t) p, sizeof(struct mfs_kmemhead));
    return;
}

/*
 * Substitute for mfs_strdup that passes through caller
 * of strdup, not the pc of strdup itself!
 */

char *
mfs_kmstrdup(
    char *str,
    caddr_t ra,
    caddr_t rara
)
{
    char *rp = mfs_kalloc(STRLEN(str)+1, KM_SLEEP, ra, rara);
    if (rp != NULL) STRCPY(rp, str);
    return(rp);
}

#endif /* KMEMDEBUG */

/*********************************************************************
 *  Additional KMEM debug "hooks" to keep a trace buffer
 *  of allocated memory 
 *
 */

#ifdef MVFS_KMEMTRACE
typedef struct kmem_w_ent {
    void *block;
    int	size;
    int data1;		/* For extra data */
    int data2;
} kmem_w_ent_t;

kmem_w_ent_t *mvfs_kmem_w_array;
int mvfs_kmem_w_size = 0;
SPLOCK_T mvfs_ktrace_splock;

#define KMEM_TRACE_SIZE 10000

void
mvfs_ktrace_init(P_NONE)
{
    int i;

    INITSPLOCK(mvfs_ktrace_splock,"mvfs_ktrace_spl");
    mvfs_kmem_w_array = 
	KMEM_ALLOC(sizeof(kmem_w_ent_t) * KMEM_TRACE_SIZE, KM_SLEEP);
    if(mvfs_kmem_w_array == NULL) {
	MVFS_PRINTF("mvfs_ktrace_init: couldnt allocate array\n");
        return;
    }

    for (i=0; i<KMEM_TRACE_SIZE; i++)
        mvfs_kmem_w_array[i].block = NULL;
}

void
mvfs_ktrace_unload(P_NONE)
{
    int i;

    FREESPLOCK(mvfs_ktrace_splock);
    if (mvfs_kmem_w_array != NULL) {
        KMEM_FREE(mvfs_kmem_w_array, sizeof(kmem_w_ent_t) * KMEM_TRACE_SIZE);
    }
}
  
VOID     
mvfs_ktrace_alloc(block, size, data1, data2)
void *block;
int size;
int data1;	/* Extra data - usually return PC */
int data2;
{
    int i;
    SPL_T s;

    if(block != NULL) {
        SPLOCK(mvfs_ktrace_splock, s);
	for (i=0;i<KMEM_TRACE_SIZE;i++) {
	    if (mvfs_kmem_w_array[i].block == NULL) {
	        mvfs_kmem_w_array[i].block = block;
		mvfs_kmem_w_array[i].size = size;
		mvfs_kmem_w_array[i].data1 = data1;
                mvfs_kmem_w_array[i].data2 = data2;
		break;
	    }
	}
        SPUNLOCK(mvfs_ktrace_splock, s);
    }
    return;
}

VOID     
mvfs_ktrace_free(block)
void *block;
{
    int i;
    SPL_T s;

    if (block != NULL) {
        SPLOCK(mvfs_ktrace_splock, s);
        for(i=0;i<KMEM_TRACE_SIZE;i++) {
	    if(mvfs_kmem_w_array[i].block == block) {
		mvfs_kmem_w_array[i].block = NULL;
		break;
	    }
	}
        SPUNLOCK(mvfs_ktrace_splock, s);
    }
    return;
}

/* Random trace information in a separate trace buffer */

int *mvfs_tracebuffer;
int *mvfs_trace_p;

VOID
mvfs_trace(
    IN int data1,
    IN int data2,
    IN int data3,
    IN int data4
    )
{
    SPL_T s;
    int *trace_p;

    /* Allocate tracebuffer on first call */

    if (mvfs_tracebuffer == NULL) {
        trace_p = (int *)KMEM_ALLOC(MVFS_TRACEBUFFER_SIZE, KM_SLEEP);
        SPLOCK(mvfs_ktrace_splock, s);
        if (mvfs_tracebuffer == NULL) {
            mvfs_tracebuffer = trace_p;
            mvfs_trace_p = trace_p;
            SPUNLOCK(mvfs_ktrace_splock, s);
        } else {
            SPUNLOCK(mvfs_ktrace_splock, s);
            if (trace_p != NULL) KMEM_FREE(trace_p, MVFS_TRACEBUFFER_SIZE);
        }
    }

    SPLOCK(mvfs_ktrace_splock, s);

    /* If still no buffer, then just return */

    if (mvfs_trace_p == NULL) {
        SPUNLOCK(mvfs_ktrace_splock, s);
        return;
    }

    /* Write the next entry */

    *mvfs_trace_p++ = data1;
    *mvfs_trace_p++ = data2;
    *mvfs_trace_p++ = data3;
    *mvfs_trace_p++ = data4;

    /* Wrap to beginning if needed */

    if (mvfs_trace_p >= mvfs_tracebuffer + 
                                (MVFS_TRACEBUFFER_SIZE/sizeof(int))) {
        mvfs_trace_p = mvfs_tracebuffer;
    }

    SPUNLOCK(mvfs_ktrace_splock, s);
    return;
}
#endif /* MVFS_KMEMTRACE */

caddr_t
mvfs_slab_getchunk(
    struct mvfs_slab_list *slistp,
    unsigned int size
)
{
    SPL_T s;
    mvfs_aligner_t *tp;
    mvfs_slab_footer_t *slp;
    mvfs_slab_footer_t *new_slp = NULL;
    short traced = 0;
    unsigned int req_size;

    MDB_XLOG((MDB_MEMOP,"getchunk slab list %"KS_FMT_PTR_T"\n", slistp));
    SPLOCK(slistp->mvfs_slablock, s);
    ASSERT(slistp->slb_eltpayload_size == size); /* make sure caller wants
						right size */
    while (1) {
	/* keep going until we get one */
        if (slistp->slb_tot_freecount == 0) {
            /* There is no free element in any slab. Get a new slab. */
            traced = slistp->slb_traced;
            req_size = slistp->slb_eltpayload_size;
            SPUNLOCK(slistp->mvfs_slablock, s);

            new_slp = mvfs_new_slab_int(&req_size, traced);
            if (new_slp == NULL) {
                MDB_XLOG((MDB_MEMOP,"Slab page allocation failed.\n"));
                return(NULL);
            }
            MDB_XLOG((MDB_MEMOP,"newchunk footer %"KS_FMT_PTR_T" oldfoot %"KS_FMT_PTR_T"\n",
			    new_slp, slistp->slb_head));
            SPLOCK(slistp->mvfs_slablock, s);

            /* While we were waiting for allocation of a new slab,
             * others may have come in and either allocated a slab
             * or freed up elements from the existing slabs.
             * We will keep our slab as long as there are less than 
             * one slab worth of free elements in the list. This is 
             * to prevent cases where we went through all of the 
             * trouble of allocating a slab just to give it up for 
             * a few elements that may have been freed behind our 
             * back and that may rapidly get consumed on a growing list.
             */
            if (slistp->slb_tot_freecount < new_slp->slb_freecount) {
                /* TODO: Do we want want to put this slab at the *end* of the
                 * slab chain?  Doing so would mean slab chain traversals to
                 * find a free chunk, but might also mean better packing in
                 * the early slabs and the opportunity to deal well with
                 * temporary high-water marks if long-lived processes are
                 * frequent.
                 */
                slp = new_slp;
                new_slp = NULL; /* we will use this slab so don't free */
                slp->slb_nextslab = slistp->slb_head;
                slistp->slb_head->slb_prevslab = slp;
                slp->slb_prevslab = NULL;
                slistp->slb_tot_eltcount += slp->slb_eltcount;
                slistp->slb_tot_freecount += slp->slb_freecount;
                slistp->slb_head = slp;
            }
        } /* if no free element in list */
        for (slp = slistp->slb_head;
             slp;
             slp = slp->slb_nextslab) 
        {
            if (slp->slb_free) {
                /* take elt out of this slab */
                ASSERT(slp->slb_freecount > 0);
                slp->slb_freecount--;
                slistp->slb_tot_freecount--;
                tp = (mvfs_aligner_t *)slp->slb_free;
                ASSERT(tp->te_flist.fre_page == slp);
                slp->slb_free = tp->te_flist.fre_next;
                tp->te_flist.fre_next = NULL;
                if ((slp != slistp->slb_head) &&
                    (slp->slb_freecount != 0))
                {
                    /* We had to look for this slab and there are more free elements.
                     * Move it to the head of the list so that the next alloc finds
                     * it immediately.
                     */
                     slp->slb_prevslab->slb_nextslab = slp->slb_nextslab;
                     if (slp->slb_nextslab != NULL)
                         slp->slb_nextslab->slb_prevslab = slp->slb_prevslab;
                     slp->slb_prevslab = NULL;
                     slp->slb_nextslab = slistp->slb_head;
                     slistp->slb_head->slb_prevslab = slp;
                     slistp->slb_head = slp;
                }
                SPUNLOCK(slistp->mvfs_slablock, s);
                /* If we allocated a slab & never used it, then free it here */
                if (new_slp != NULL)
                    mvfs_free_slab(new_slp, traced);
                return (caddr_t) &tp->foo.align;
            }
        }
    } /* while (1) */
}

void
mvfs_slab_freechunk(
    mvfs_slab_list_t *slistp,
    caddr_t elt,
    unsigned int size
)
{
    SPL_T s;
    mvfs_aligner_t *tp = NULL;
    mvfs_slab_footer_t *slp;
    short traced;

    /* compute free chain from passed-in address by subtracting the
     * offset of the free list structure from the offset of the
     * alignment portion in the union
     */
    tp = (mvfs_aligner_t *) (elt -
	(unsigned long)((caddr_t)&tp->foo.align - (caddr_t)&tp->te_flist));

    MDB_XLOG((MDB_MEMOP,"freechunk list %"KS_FMT_PTR_T" elt %"KS_FMT_PTR_T"\n", slistp, elt));
    SPLOCK(slistp->mvfs_slablock, s);
    slp = tp->te_flist.fre_page;
    /* make sure caller is freeing right size */
    ASSERT(slistp->slb_eltpayload_size == size);
    /* no frees on totally free page */
    ASSERT(slp->slb_freecount < slp->slb_eltcount);
    /* make sure tp is on this page */
    ASSERT(((caddr_t)slp) - slp->slb_baseoffset <= (caddr_t)tp);

    /* chain this one onto page's free list */
    tp->te_flist.fre_next = slp->slb_free;
    ASSERT((caddr_t)tp == (caddr_t)&tp->te_flist);
    slp->slb_free = &tp->te_flist;
    slp->slb_freecount++;
    slistp->slb_tot_freecount++;

    ASSERT(slistp->slb_tot_freecount >= slp->slb_freecount);

    if (slp->slb_freecount == slp->slb_eltcount &&
        slistp->slb_tot_freecount > slp->slb_freecount)
    {
	/* A totally free page.  And there is at least another slab in
	 * the slab list with free elements, so free this totally 
         * free slab. 
         */
        if (slp->slb_nextslab != NULL)
            slp->slb_nextslab->slb_prevslab = slp->slb_prevslab;
        if (slp->slb_prevslab != NULL)
            slp->slb_prevslab->slb_nextslab = slp->slb_nextslab;
        if (slistp->slb_head == slp)
            slistp->slb_head = slp->slb_nextslab;
        slistp->slb_tot_freecount -= slp->slb_freecount;
        slistp->slb_tot_eltcount -= slp->slb_eltcount;
        slp->slb_nextslab = slp->slb_prevslab = NULL;
        traced = slistp->slb_traced;
        
        SPUNLOCK(slistp->mvfs_slablock, s);
        mvfs_free_slab(slp, traced);
        MDB_XLOG((MDB_MEMOP,"free_slab footer was %"KS_FMT_PTR_T", now %"KS_FMT_PTR_T"\n",
            slp, slistp->slb_head));
        return;
    } 
    /* Otherwise if this is not the head of the list and the head slab 
     * is all full, move this slab to the head so that it can be found
     * immediately by the next alloc.
     */
    if (slistp->slb_head != slp && slistp->slb_head->slb_freecount == 0) 
    {
        if (slp->slb_nextslab != NULL)
            slp->slb_nextslab->slb_prevslab = slp->slb_prevslab;
        slp->slb_prevslab->slb_nextslab = slp->slb_nextslab;
        slp->slb_nextslab = slistp->slb_head;
        slp->slb_prevslab = NULL;
        slistp->slb_head->slb_prevslab = slp;
        slistp->slb_head = slp;
    }
    SPUNLOCK(slistp->mvfs_slablock, s);
    return;
}

/*
 * Create a slab list. Initialize its lock, allocate and initialize a 
 * slab page and hang off of the list head.
 */
mvfs_slab_list_t *
mvfs_create_slablist(
    unsigned int size,
    short traced,
    char *lock_name
)
{
    mvfs_slab_list_t *slist = NULL;
    unsigned int actual_size = size; /* init to size asked for */

    /* The size passed in needs to be at least mvfs_slab_freelist
     * to build a link list of free elements.
     * check to ensure that.
     */
    if (size < sizeof(struct mvfs_slab_freelist))
       MDKI_PANIC("mvfs_create_slablist: size < mvfs_slab_freelist");

#ifdef KMEMDEBUG
    if (traced == FALSE)
        slist = (mvfs_slab_list_t *) REAL_KMEM_ALLOC(sizeof(mvfs_slab_list_t),
                                                     KM_SLEEP);
    else
#endif
    slist = (mvfs_slab_list_t *) KMEM_ALLOC(sizeof(mvfs_slab_list_t), 
                                            KM_SLEEP);
    if (slist == NULL)
       MDKI_PANIC("mvfs_create_slablist: no mem to allocate list");

    slist->slb_head = mvfs_new_slab_int(&actual_size, traced);
    if (slist->slb_head == NULL) 
        MDKI_PANIC("mvfs_create_slablist: no mem to allocate slab page.");
    slist->slb_eltsize = actual_size;
    slist->slb_eltpayload_size = size;
    slist->slb_tot_eltcount = slist->slb_head->slb_eltcount;
    slist->slb_tot_freecount = slist->slb_head->slb_freecount;
    slist->slb_traced = traced;
    INITSPLOCK(slist->mvfs_slablock, lock_name);

    return slist;
}

/* Destroys the input slablist, freeing individual slabs attached and the 
 * associated lock.
 */

void 
mvfs_destroy_slablist(mvfs_slab_list_t *slist)
{
    mvfs_slab_footer_t *slab, *next_slab;

    slab = slist->slb_head;

    while (slab != NULL) {
         next_slab = slab->slb_nextslab;
         slab->slb_nextslab = slab->slb_prevslab = NULL;
         mvfs_free_slab(slab, slist->slb_traced);
         slab = next_slab;
    }

    FREESPLOCK(slist->mvfs_slablock);
#ifdef KMEMDEBUG
    if (slist->slb_traced == FALSE)
	REAL_KMEM_FREE(slist, sizeof(mvfs_slab_list_t));
    else
#endif
    KMEM_FREE(slist, sizeof(mvfs_slab_list_t));
}

/* Called to initialize a new slab that will be added to a slab list */

STATIC mvfs_slab_footer_t *
mvfs_new_slab_int(
    unsigned int *payload_size, /* IN req size, OUT actual size */
    int traced
)
{
    caddr_t slab;
    mvfs_slab_footer_t *footer;
    struct mvfs_slab_freelist *flbase;
    register int i;
    int remainder;
    int size; /* it's total element size: header + payload_size */

    /* increase the size by MVFS_MAX_ALIGN_T for foo_spacer */
    size = sizeof(MVFS_MAX_ALIGN_T) + *payload_size;
    /* increase the size to multiple of MVFS_MAX_ALIGN_T */
    if((remainder = size % sizeof(MVFS_MAX_ALIGN_T)) != 0)
        size = size - remainder + sizeof(MVFS_MAX_ALIGN_T);

#ifdef KMEMDEBUG
    if (traced == FALSE)
	slab = REAL_KMEM_ALLOC(MVFS_SLABSIZE, KM_SLEEP);
    else
#endif
    slab = KMEM_ALLOC(MVFS_SLABSIZE, KM_SLEEP);
    if (slab == NULL)
        return NULL;

    footer = (mvfs_slab_footer_t *)((slab + MVFS_SLABSIZE) - sizeof(*footer));
    MDB_XLOG((MDB_MEMOP,"new_slab elt size %d footer %"KS_FMT_PTR_T"\n", size, footer));
    footer->slb_nextslab = NULL;
    footer->slb_prevslab = NULL;
    footer->slb_baseoffset = (u_short)((caddr_t) footer - slab);
    footer->slb_freecount = footer->slb_eltcount =
	(MVFS_SLABSIZE - sizeof(*footer)) / size;

    flbase = (struct mvfs_slab_freelist *)slab;
    footer->slb_free = flbase;
    for (i = 0; i < footer->slb_eltcount-1; i++) {
	flbase->fre_page = footer;
	flbase->fre_next = (struct mvfs_slab_freelist *)((caddr_t)flbase + size);
	flbase = flbase->fre_next;
    }
    flbase->fre_page = footer;
    flbase->fre_next = NULL;
    *payload_size = size; /* return actual size used to carve elements */

    return footer;
}

STATIC void
mvfs_free_slab(
    mvfs_slab_footer_t *footer,
    short traced
)
{
    caddr_t freeaddr;

    /* can't print here---caller may hold spinlock */
    /* MDB_XLOG((MDB_MEMOP,"free_slab footer %"KS_FMT_PTR_T"\n", footer)); */
    ASSERT(footer->slb_freecount == footer->slb_eltcount);
    ASSERT(footer->slb_nextslab == NULL);
    ASSERT(footer->slb_prevslab == NULL);
    freeaddr = ((caddr_t)footer) - footer->slb_baseoffset;
#ifdef KMEMDEBUG
    if (traced == FALSE)
	REAL_KMEM_FREE(freeaddr, MVFS_SLABSIZE);
    else
#endif
    KMEM_FREE(freeaddr, MVFS_SLABSIZE);
}

#endif /* !MVFS_SYSTEM_KMEM */
static const char vnode_verid_mvfs_kmem_c[] = "$Id:  afe3720c.66b911dc.9bbb.00:01:83:09:5e:0d $";
