package main

import (
	"fmt"
	"math/rand"
	"sync"
	"time"
)

type triple struct {
	nexthop int
	cost    int
	changed bool
}

type pair struct {
	j    int
	cost int
}

type packet struct {
	l int
	p []*pair
}

type table struct {
	t []*triple
}

type addr struct {
	r int
	h int
}

type std_packet struct {
	s       *addr
	r       *addr
	visited []int
}

func createAddr(router int, host int) *addr {

	newAddr := addr{r: router, h: host}
	return &newAddr
}

func createstd_packet(sender *addr, receiver *addr) *std_packet { 

	var list []int

	newstd_packet := std_packet{s: sender, r: receiver, visited: list}
	return &newstd_packet
}

func createTriple(n int, c int, flag bool) *triple {

	new := triple{nexthop: n, cost: c, changed: flag}
	return &new
}

func createPair(j int, cost int) *pair {

	new := pair{j: j, cost: cost}
	return &new
}

func createPacket(l int, p []*pair) *packet {

	new := packet{l: l, p: p}
	return &new
}

func std_packetToStr(p *std_packet) string {

	s_addr := p.s
	r_addr := p.r
	visited_routers := p.visited

	msg := fmt.Sprintf("nadawca - (%d, %d), odbiorca - (%d, %d), odwiedzone routery - %v", s_addr.r,
		s_addr.h,
		r_addr.r,
		r_addr.h,
		visited_routers)

	return msg
}

func routingTableToStr(rTable *table, myId int) string {

	result := ""

	for idx, tr := range rTable.t {

		if idx == myId {

			continue

		}

		result += fmt.Sprintf("%d - (%d, %d, %t) ", idx, tr.nexthop,
			tr.cost,
			tr.changed)

	}

	return result
}

func newMutex() *sync.Mutex {

	var m sync.Mutex
	return &m
}

func initialize(i int, neighbours []int, n int) *table {

	new := make([]*triple, n)

	for j := 0; j < n; j++ {

		if i == j {
			continue
		}

		if search(neighbours, j) {

			v := createTriple(j, 1, true)
			new[j] = v

		} else {

			cost := myAbs(i - j)

			if i < j {

				v := createTriple(i+1, cost, true)
				new[j] = v

			} else if j < i {

				v := createTriple(i-1, cost, true)
				new[j] = v
			}

		}
	}

	tab := table{t: new}
	return &tab

}

func search(slice []int, vertice int) bool {

	for _, item := range slice {

		if item == vertice {
			return true
		}
	}
	return false
}

func findElementPosition(slice []int, element int) int {

	for pos, item := range slice {

		if item == element {
			return pos
		}
	}

	return -1
}

func myAbs(v int) int {

	if v >= 0 {
		return v
	} else {
		return -v
	}
}

func merge(cs []chan *packet) <-chan *packet {
	out := make(chan *packet)
	var wg sync.WaitGroup
	wg.Add(len(cs))
	for _, c := range cs {
		go func(c <-chan *packet) {
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

func merge_std(cs []chan *std_packet) <-chan *std_packet {
	out := make(chan *std_packet)
	var wg sync.WaitGroup
	wg.Add(len(cs))
	for _, c := range cs {
		go func(c <-chan *std_packet) {
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

func host(r int, h int, bound int, existing_hosts []int, host_in chan *std_packet, host_out chan *std_packet,
	messages chan<- string) {

	n := len(existing_hosts) 

	rr := 0
	rh := 0

	for true {

		rr = rand.Intn(n)
		rh = rand.Intn(existing_hosts[rr])

		if rr != r || rh != h {
			break
		}
	}

	sen := createAddr(r, h)
	rec := createAddr(rr, rh)

	initial_packet := createstd_packet(sen, rec)

	host_out <- initial_packet

	for true {

		std_p := <-host_in

		idt := fmt.Sprintf("Host (%d, %d) dostał pakiet: ", r, h)

		msg := idt + std_packetToStr(std_p)
		messages <- msg

		rf := rand.Float64()
		time.Sleep(time.Duration(rf * float64(bound) * float64(time.Second)))

		me := createAddr(r, h)
		rec := std_p.s

		new_std_packet := createstd_packet(me, rec)

		host_out <- new_std_packet

	}

}

func vertice(id int, routingTable *table, incoming []chan *packet, outcoming []chan *packet, m *sync.Mutex, bound int,
	messages chan<- string, std_in []chan *std_packet, std_out []chan *std_packet, existing_hosts []int, myNeigh []int) {

	var forwarder_chan = make(chan *std_packet, len(existing_hosts)*100)

		
	no_hosts := existing_hosts[id]

	h_ins := make([]chan *std_packet, no_hosts)
	h_outs := make([]chan *std_packet, no_hosts)

	for i := 0; i < no_hosts; i++ {

		h_in := make(chan *std_packet)
		h_out := make(chan *std_packet)

		h_ins[i] = h_in
		h_outs[i] = h_out

		go host(id, i, bound, existing_hosts, h_in, h_out, messages)
	}

	go func() { /* sender */

		for true {

			r := rand.Float64()
			time.Sleep(time.Duration(r * float64(bound) * float64(time.Second)))

			p := make([]*pair, 0)

			m.Lock()
			for idx, tr := range routingTable.t {

				if idx == id {
					continue
				}

				if tr.changed {

					new := createPair(idx, tr.cost)
					p = append(p, new)
					tr.changed = false

				}

			}
			m.Unlock()

			if len(p) > 0 {

				packet := createPacket(id, p)

				for _, ch := range outcoming {
					ch <- packet
				}
			}
		}

	}()

	go func() { /* receiver */

		change_flag := false

		for true {

			out := merge(incoming)

			for pack := range out {

				l := pack.l

				after := fmt.Sprintf("Receiver %d - stan po modyfikacji: ", id)

				for _, pr := range pack.p {

					new_cost := 1 + pr.cost
					j := pr.j

					if id == j {
						continue
					}

					m.Lock()
					if new_cost < routingTable.t[j].cost {

						change_flag = true

						routingTable.t[j].cost = new_cost
						routingTable.t[j].nexthop = l
						routingTable.t[j].changed = true

					}
					m.Unlock()
				}

				if change_flag {

					m.Lock()

					for idx, tr := range routingTable.t {

						if idx == id {
							continue
						}

						after = after + fmt.Sprintf("%d - (%d, %d, %t) ", idx,
							tr.nexthop,
							tr.cost,
							tr.changed)

					}

					m.Unlock()

					messages <- after

				}

				change_flag = false

			}

		}

	}()

	go func() { /* receiving forwarder */

		for true {

			inputs_concat := append(std_in, h_outs...)
			inputs := merge_std(inputs_concat)

	
			for p := range inputs {

				p.visited = append(p.visited, id)

				forwarder_chan <- p
							
			}

		}

	}()

	go func() { /* sending forwarder */

		for true {

			p := <-forwarder_chan

			receiver_r := p.r.r
			h := p.r.h

			if receiver_r == id {

				h_ins[h] <- p

			} else {

				n := -1

				m.Lock()
				n = routingTable.t[receiver_r].nexthop
				m.Unlock()

				k := findElementPosition(myNeigh, n) 

				if n != -1  && k != -1 {
					std_out[k] <- p
				}

			}

		}

	}()

	for true {

	}
}

func printServer(messages <-chan string) {

	for true {

		msg := <-messages
		fmt.Println(msg)
		fmt.Println(" - - - - - - - - - - - -")
	}
}

func main() {

	var n, d, v1, v2, max_h int

	end := make(chan bool)

	msg := make(chan string)
	bound := 2

	fmt.Println("Wprowadz liczbę wierzchołków : ")
	fmt.Scanln(&n)
	fmt.Println("Wprowadz liczbę skrótów : ")
	fmt.Scanln(&d)
	fmt.Println("Podaj maksymalną liczbe hostów :")
	fmt.Scanln(&max_h)

	fmt.Println("Generuje graf nieskierowany o zadanych parametrach...")

	myGraph := make([][]int, n)
	inEdges := make([][]chan *packet, n)
	outEdges := make([][]chan *packet, n)

	std_inEdges := make([][]chan *std_packet, n)
	std_outEdges := make([][]chan *std_packet, n)

	existing_hosts := make([]int, n)

	myTables := make([]*table, n)

	for i := 0; i < n-1; i++ {

		myGraph[i] = append(myGraph[i], i+1)

		e := make(chan *packet)
		outEdges[i] = append(outEdges[i], e)
		inEdges[i+1] = append(inEdges[i+1], e)

		myGraph[i+1] = append(myGraph[i+1], i)

		e = make(chan *packet)
		outEdges[i+1] = append(outEdges[i+1], e)
		inEdges[i] = append(inEdges[i], e)

		e_std := make(chan *std_packet)

		std_outEdges[i] = append(std_outEdges[i], e_std)
		std_inEdges[i+1] = append(std_inEdges[i+1], e_std)

		e_std = make(chan *std_packet)
		
		std_outEdges[i+1] = append(std_outEdges[i+1], e_std)
		std_inEdges[i] = append(std_inEdges[i], e_std)

	}

	for i := 0; i < n; i++ {
		existing_hosts[i] = rand.Intn(max_h) + 1
	}

	for i := 0; i < d; i++ {

		for {

			v1 = rand.Intn(n)
			v2 = rand.Intn(n)

			found := search(myGraph[v1], v2)

			if !found && v1 != v2 {
				break
			}

		}

		myGraph[v1] = append(myGraph[v1], v2)

		e := make(chan *packet)
		outEdges[v1] = append(outEdges[v1], e)
		inEdges[v2] = append(inEdges[v2], e)

		myGraph[v2] = append(myGraph[v2], v1)

		e = make(chan *packet)
		outEdges[v2] = append(outEdges[v2], e)
		inEdges[v1] = append(inEdges[v1], e)

		e_std := make(chan *std_packet)

		std_outEdges[v1] = append(std_outEdges[v1], e_std)
		std_inEdges[v2] = append(std_inEdges[v2], e_std)

		e_std = make(chan *std_packet)

		std_outEdges[v2] = append(std_outEdges[v2], e_std)
		std_inEdges[v1] = append(std_inEdges[v1], e_std)

	}

	fmt.Println("Graf wygenerowany. Drukuje liste sąsiedztwa...")

	for i, list := range myGraph {
		fmt.Printf("Wierzchołek %d | ", i)
		fmt.Printf("Liczba hostów: %d | ", existing_hosts[i])
		fmt.Printf("Sąsiedzi : ")
		fmt.Println(list)
	}

	fmt.Println("Rozpoczynam symulacje...")

	for i := 0; i < n; i++ {

		tab := initialize(i, myGraph[i], n)
		myTables[i] = tab
		m := newMutex()
		go vertice(i, tab, inEdges[i], outEdges[i], m, bound, msg, std_inEdges[i], std_outEdges[i], existing_hosts, myGraph[i])

	}

	go printServer(msg)

	<-end

//	<-time.After(5 * time.Second)

//	fmt.Println("Symulacja zakończona...")

}
