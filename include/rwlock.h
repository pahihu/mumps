#ifndef _MUMPS_RWLOCK_H_
#define _MUMPS_RWLOCK_H_

void lock_reader(volatile u_int *lck);
void unlock_reader(volatile u_int *lck);
void lock_writer(volatile u_int *lck);
void unlock_writer(volatile u_int *lck);
void unlock_writer_to_reader(volatile u_int *lck);

short TrySemLock(int sem_num, int numb);
short SemLock(int sem_num, int numb);
short SemUnlock(int sem_num, int numb);

#endif
