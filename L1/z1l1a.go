package main

import (
	"fmt"
	"math/rand"
	"reflect"
	"strconv"
	"time"
)

func sender(input chan<- int, k int, bound int) {

	for i := 0; i < k; i++ {

		r := rand.Float64()
		time.Sleep(time.Duration(r * float64(time.Second) * float64(bound)))

		input <- i
	}

	close(input)
}

func receiver(output <-chan int, end chan<- bool, packets chan<- int, vers chan<- int, strs chan<- string, k int,
	bound int, receivedPackets []int) {

	for p := range output {

		r := rand.Float64()
		time.Sleep(time.Duration(r * float64(time.Second) * float64(bound)))

		receivedPackets = append(receivedPackets, p)

		strs <- "REC"
		packets <- p
		vers <- -1

		
		if len(receivedPackets) == k {

			end <- true

		}
	}

}

func vertice(id int, successors []chan int, predecessors []chan int, packets chan<- int, vers chan<- int, strs chan<- string,
	bound int) {

	for true {

		cases := make([]reflect.SelectCase, len(predecessors))
		for i, ch := range predecessors {
			cases[i] = reflect.SelectCase{Dir: reflect.SelectRecv, Chan: reflect.ValueOf(ch)}
		}
		_, value, _ := reflect.Select(cases)
		ptostr := value.String()

		p, _ := strconv.Atoi(ptostr)

		packets <- p
		vers <- id
		strs <- "IN"

		r := rand.Float64()
		time.Sleep(time.Duration(r * float64(time.Second) * float64(bound)))

		next := rand.Intn(len(successors))
		successors[next] <- p

	}

}

func printingRoutine(verChannel <-chan int, packetChannel <-chan int, strChannel <-chan string,
	visitedVertices [][]int, servicedPackets [][]int) {

	for true {

		packet := <-packetChannel
		ver := <-verChannel
		info := <-strChannel

		if info == "IN" {

			msg := fmt.Sprintf("Pakiet %d jest w wierzchołku %d", packet, ver)
			fmt.Println(msg)
			fmt.Printf("\n")

			visitedVertices[packet] = append(visitedVertices[packet], ver)
			servicedPackets[ver] = append(servicedPackets[ver], packet)

		} else {

			msg := fmt.Sprintf("Odebrałem pakiet %d", packet)
			fmt.Println(msg)
			fmt.Printf("\n")
		}

	}

}

func search(slice []int, vertice int) bool {

	for _, item := range slice {

		if item == vertice {
			return true
		}
	}
	return false
}

func main() {

	var n, d, k, v1, v2 int

	var receivedPackets []int

	bound := 1

	end := make(chan bool)
	inputChanel := make(chan int)
	outputChanel := make(chan int)
	verChannel := make(chan int)
	packetChannel := make(chan int)
	strChannel := make(chan string)

	rand.Seed(time.Now().UnixNano())

	fmt.Println("Podaj liczbę wierzchołków: ")
	fmt.Scanln(&n)
	fmt.Println("Podaj liczbę skrótów: ")
	fmt.Scanln(&d)
	fmt.Println("Podaj liczbę pakietów: ")
	fmt.Scanln(&k)

	directedGraph := make(map[int][]int)
	channelEdges := make(map[int][]chan int)
	predecessorsEdges := make(map[int][]chan int)

	visitedVertices := make([][]int, k)
	servicedPackets := make([][]int, n)

	predecessorsEdges[0] = append(predecessorsEdges[0], inputChanel)

	for i := 0; i < n-1; i++ {

		directedGraph[i] = append(directedGraph[i], i+1)

		newChan := make(chan int)

		channelEdges[i] = append(channelEdges[i], newChan)
		predecessorsEdges[i+1] = append(predecessorsEdges[i+1], newChan)

	}

	for i := 0; i < d; i++ {

		for {

			v1 = rand.Intn(n - 1)
			v2 = rand.Intn(n)

			found := search(directedGraph[v1], v2)

			if !found && v1 < v2 {
				break
			}

		}

		directedGraph[v1] = append(directedGraph[v1], v2)

		newChan := make(chan int)

		channelEdges[v1] = append(channelEdges[v1], newChan)
		predecessorsEdges[v2] = append(predecessorsEdges[v2], newChan)

	}

	channelEdges[n-1] = append(channelEdges[n-1], outputChanel)

	fmt.Println("Graf wygenerowany. Drukuje...")
	fmt.Printf("\n")

	for i := 0; i < n; i++ {

		fmt.Printf("Wierzchołek: %d ", i)
		fmt.Printf("Sąsiedzi: ")
		fmt.Println(directedGraph[i])
	}

	fmt.Println("Rozpoczynam symulacje...")

	go sender(inputChanel, k, bound)
	go receiver(outputChanel, end, packetChannel, verChannel, strChannel, k, bound, receivedPackets)
	go printingRoutine(verChannel, packetChannel, strChannel, visitedVertices, servicedPackets)

	for i := 0; i < n; i++ {

		go vertice(i, channelEdges[i], predecessorsEdges[i], packetChannel, verChannel, strChannel, bound)

	}

	<-end

	fmt.Println("Symulacja zakończona. Drukuje raporty końcowe...")
	fmt.Printf("\n")

	fmt.Printf("Lista odwiedzonych wierzchołków przez pakiety: \n")

	for i := 0; i < k; i++ {

		fmt.Printf("Pakiet: %d ", i)
		fmt.Printf("Wierzchołki: ")
		fmt.Println(visitedVertices[i])
		fmt.Printf("\n")
	}

	fmt.Printf("Lista pakietów obsłużonych przez poszczególne wierzchołki: \n")

	for i := 0; i < n; i++ {

		fmt.Printf("Wierzchołek: %d ", i)
		fmt.Printf("Pakiety: ")
		fmt.Println(servicedPackets[i])
		fmt.Printf("\n")
	}

}
