package main

import "fmt"
import "time"

var i = 0

func task_a(ch chan int) {
	for j := 0; j < 1000000; j++ {
		i++
	}
	ch <- 0
}

func task_b(ch chan int) {
	for j := 0; j < 1000000; j++ {
		i--
	}
	ch <- 0
}

func main() {

	//task_a()
	//task_b()

	ch := make(chan int)
	go task_a(ch)
	go task_b(ch)

	<-ch
	<-ch

	i <- i_ch 
	i_ch <- i

	fmt.Println(i)

}
