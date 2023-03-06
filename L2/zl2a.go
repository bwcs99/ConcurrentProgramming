package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

type pack struct {
	id  int
	ttl int
}

type message struct {
	p   int
	v   int
	msg string
}

func newPack(id int, ttl int) *pack {

	p := pack{id: id, ttl: ttl}
	return &p

}

func newMsg(p int, v int, msg string) *message {

	nmsg := message{p: p, v: v, msg: msg}
	return &nmsg
}

func convert(channels ...chan *pack) []<-chan *pack {

	ret := make([]<-chan *pack, len(channels))

	for i, ch := range channels {
		ret[i] = ch
	}

	return ret
}

func merge(cs []<-chan *pack) <-chan *pack {
	out := make(chan *pack)
	var wg sync.WaitGroup
	wg.Add(len(cs))
	for _, c := range cs {
		go func(c <-chan *pack) {
			for v := range c {
				out <- v
			}
			wg.Done()
		}(c)
	}
	go func() {
		wg.Wait()
		close(out)
	}()
	return out
}

func trySend(sendChan chan *pack, packet *pack) bool {

	select {
	case sendChan <- packet:
		return true
	default:
		return false
	}
}

func knuthShuffle(chans []chan *pack) []chan *pack {

	rand.Seed(time.Now().UnixNano())
	for i := len(chans) - 1; i >= 1; i-- {

		j := rand.Intn(i + 1)

		chans[i], chans[j] = chans[j], chans[i]
	}

	return chans
}

func hunter(trap []chan int, bound int, msgChan chan *message) {

	for true {

		r := rand.Float64()
		time.Sleep(time.Duration(r * float64(time.Second) * float64(bound)))

		n := rand.Intn(len(trap))
		trap[n] <- 1

	}

}

func sender(successors []chan *pack, k int, bound int, msgChan chan *message, h int, myTrap chan int) {

	for i := 0; i < k; i++ {

		r := rand.Float64()
		time.Sleep(time.Duration(r * float64(time.Second) * float64(bound)))

		select{
		case <-myTrap:
			msg := newMsg(i, 0, "TR")
			msgChan <- msg
		default:

			if h == 0 {

				msg := newMsg(i, 0, "DE")
				msgChan <- msg
	
			} else {
	
				msg := newMsg(i, 0, "IN")
				msgChan <- msg
	
				p := newPack(i, h)
				p.ttl -= 1
	
				n := rand.Intn(len(successors))
				successors[n] <- p
	
			}

		}

			
	}
}

func receiver(id int, predecessors []chan *pack, bound int, msgChan chan *message, myTrap <-chan int) {

	for true {

		select {
		case _ = <-myTrap:

			conv := convert(predecessors...)
			out := merge(conv)

			for packet := range out {

				msg := newMsg(packet.id, id, "TR")
				msgChan <- msg

				r := rand.Float64()
				time.Sleep(time.Duration(r * float64(time.Second) * float64(bound)))

			}

		default:

			conv := convert(predecessors...)
			out := merge(conv)

			for packet := range out {

				msg := newMsg(packet.id, id, "RE")
				msgChan <- msg

				r := rand.Float64()
				time.Sleep(time.Duration(r * float64(time.Second) * float64(bound)))

			}

		}

	}
}

func forwarder(id int, predecessors []chan *pack, successors []chan *pack, bound int, msgChan chan *message,
	myTrap <-chan int) {

	for true {

		select {
		case _ = <-myTrap:

			conv := convert(predecessors...)
			out := merge(conv)

			msg := newMsg(0, 0, fmt.Sprintf("Jestem wierzchołek %d i mam pułapke", id))
			msgChan <- msg

			for p := range out {

				msg := newMsg(p.id, id, "TR")
				msgChan <- msg

				r := rand.Float64()
				time.Sleep(time.Duration(r * float64(time.Second) * float64(bound)))

			}

		default:

			conv := convert(predecessors...)
			out := merge(conv)

			for p := range out {

				if p.ttl == 0 {

					msg := newMsg(p.id, id, "DE")
					msgChan <- msg

					r := rand.Float64()
					time.Sleep(time.Duration(r * float64(time.Second) * float64(bound)))

				} else {

					msg := newMsg(p.id, id, "IN")
					msgChan <- msg

					r := rand.Float64()
					time.Sleep(time.Duration(r * float64(time.Second) * float64(bound)))

					p.ttl -= 1

					flag := false
					shuffled := knuthShuffle(successors)


					for i := 0 ; i < 1000 ; i++ {

						for _,c := range shuffled {

							flag = trySend(c, p)

							if flag == true{
								break
							}
						}

						if flag == true {
							break
						} else {

							r := rand.Float64()
							time.Sleep(time.Duration(r * float64(time.Second) * float64(bound)))

						}
					}

				}

			}

		}
	}

}

func server(msgChan chan *message, endChan chan bool, visited [][]int, serviced [][]int, vector []int, k int) {

	for true {

		info := <-msgChan

		packet := info.p
		vertice := info.v
		msgType := info.msg

		if msgType == "IN" {

			msg := fmt.Sprintf("Pakiet %d jest w wierzchołku %d", packet, vertice)
			fmt.Println(msg)

		} else if msgType == "RE" {

			vector = append(vector, packet)

			msg := fmt.Sprintf("Odebrałem pakiet %d", packet)
			fmt.Println(msg)

		} else if msgType == "DE" {

			vector = append(vector, packet)

			msg := fmt.Sprintf("Pakiet %d umarł w wierzchołku %d", packet, vertice)
			fmt.Println(msg)

		} else if msgType == "TR" {

			vector = append(vector, packet)

			msg := fmt.Sprintf("Pakiet %d wpadł w pułapke w wierzchołku %d", packet, vertice)
			fmt.Println(msg)

		} else {
			msg := msgType
			fmt.Println(msg)
		}

		visited[packet] = append(visited[packet], vertice)
		serviced[vertice] = append(serviced[vertice], packet)

		if len(vector) == k {
			endChan <- true
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

	var n, k, h int
	var receivedPackets []int

	edgeCount := 0

	bound := 1

	msgChan := make(chan *message)
	end := make(chan bool)

	rand.Seed(time.Now().UnixNano())

	fmt.Println("Podaj liczbę wierzchołków: ")
	fmt.Scanln(&n)
/*	fmt.Println("Podaj liczbę skrótów: ")
	fmt.Scanln(&d) */
	fmt.Println("Podaj liczbę pakietów: ")
	fmt.Scanln(&k)
/*	fmt.Println("Podaj liczbę krawędzi powrotnych: ")
	fmt.Scanln(&b) */
	fmt.Println("Podaj czas życia każdego pakietu: ")
	fmt.Scanln(&h)

	directedGraph := make(map[int][]int)
	successors := make(map[int][]chan *pack)
	predecessors := make(map[int][]chan *pack)
	trapsChan := make([]chan int, n)

	for i := 0; i < n; i++ {

		trapsChan[i] = make(chan int)
	}

	servicedPackets := make([][]int, n)
	visitedVertices := make([][]int, k)

	for i := 0; i < n-1; i++ {

		for j := i ; j < n-1 ; j++ {

			directedGraph[i] = append(directedGraph[i], j+1)

			newChan := make(chan *pack)

			successors[i] = append(successors[i], newChan)
			predecessors[j+1] = append(predecessors[j+1], newChan)

			edgeCount++


		}
	}

	for i := n-2 ; i >= 1 ; i-- {

		for j := i ; j > 1 ; j-- {

			directedGraph[i] = append(directedGraph[i], j-1)

			newChan := make(chan *pack)

			successors[i] = append(successors[i], newChan)
			predecessors[j-1] = append(predecessors[j-1], newChan)

			edgeCount++

		}
	}

	fmt.Println("Graf wygenerowany. Drukuje...")
	fmt.Printf("\n")

	fmt.Printf("Liczba krawędzi w grafie: %d\n", edgeCount)
	fmt.Printf("Procentowa zawartość krawędzi w grafie: %f\n", float64(edgeCount)/float64(n*(n-1)))

	for i := 0; i < n; i++ {

		fmt.Printf("Wierzchołek: %d ", i)
		fmt.Printf("Sąsiedzi: ")
		fmt.Println(directedGraph[i])
	}

	fmt.Println("Rozpoczynam symulacje...")

	
	for i := 1; i < n-1; i++ {

		go forwarder(i, predecessors[i], successors[i], bound, msgChan, trapsChan[i])
	}

	go sender(successors[0], k, bound, msgChan, h, trapsChan[0])
	go receiver(n-1, predecessors[n-1], bound, msgChan, trapsChan[n-1])
	go server(msgChan, end, visitedVertices, servicedPackets, receivedPackets, k)
	go hunter(trapsChan, bound, msgChan)

	<-end

	fmt.Println("Symulacja zakończona. Drukuje raporty końcowe...")
	fmt.Printf("\n")

	for p, vs := range visitedVertices {

		fmt.Printf("Pakiet %d | Odwiedzone wierzchołki: ", p)
		fmt.Println(vs)
	}

	fmt.Printf("\n")

	for v, ps := range servicedPackets {

		fmt.Printf("Wierzchołek %d | Obsłużone pakiety: ", v)
		fmt.Println(ps)
	}

}

