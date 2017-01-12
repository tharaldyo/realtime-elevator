# code.py

import threading

i = 0

def task_a():
    global i
    
    for j in range(0, 1000000):
        i += 1

def task_b():
    global i

    for j in range(0, 1000000):
        i -= 1

thread1 = threading.Thread(target=task_a)
thread2 = threading.Thread(target=task_b)

thread1.start()
thread2.start()

thread1.join()
thread2.join()

print i
