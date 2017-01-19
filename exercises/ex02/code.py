# code.py

import threading

i = 0

lock = threading.Lock()

def task_a():
    global i
    
    for j in range(0, 1000001):
        lock.acquire()
        i += 1
        lock.release()

def task_b():
    global i

    for j in range(0, 1000000):
        lock.acquire()
        i -= 1
        lock.release()

thread1 = threading.Thread(target=task_a)
thread2 = threading.Thread(target=task_b)

thread1.start()
thread2.start()

thread1.join()
thread2.join()

print i
