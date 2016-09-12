#ifndef _MUMPS_RWLOCK_H_
#define _MUMPS_RWLOCK_H_

typedef volatile u_int rwlock_t;

void lock_reader(rwlock_t *lck);
void unlock_reader(rwlock_t *lck);
void lock_writer(rwlock_t *lck);
void unlock_writer(rwlock_t *lck);
void unlock_writer_to_reader(rwlock_t *lck);

short TrySemLock(int sem_num, int numb);
short SemLock(int sem_num, int numb);
short SemUnlock(int sem_num, int numb);

#endif
