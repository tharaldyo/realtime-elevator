package main

import "fmt"
import "time"

var i = 0

func task_a(ch chan int, ch_i chan int) {
	for j := 0; j < 1000001; j++ {
		i = <-ch_i
		i++
		ch_i <- i
	}
	ch <- 0
}

func task_b(ch chan int, ch_i chan int) {
	for j := 0; j < 1000000; j++ {
		i = <-ch_i
		i--
		ch_i <- i
	}
	ch <- 0
}

func main() {

	//task_a()
	//task_b()

	ch := make(chan int)
	ch_i := make(chan int)

	//go task_a(ch, ch_i)
	//go task_b(ch, ch_i)

	ch_i <- i

	<-ch
	<-ch

	time.Sleep(500 * time.Millisecond)
	out := <-ch_i

	//fmt.Println(i)
	fmt.Println(out)
	fmt.Println("done")

}
