#!/usr/bin/ruby -w

class FiniteAutomaton
    @@nextID = 0	# shared across all states
    attr_reader:state, :start, :final, :alphabet, :transition

    #---------------------------------------------------------------
    # Constructor for the FA
    def initialize
        @start = nil 		# start state 
        @state = { } 		# all states
        @final = { } 		# final states
        @transition = { }	# transitions
        @alphabet = [ ] 	# symbols on transitions
    end

    #---------------------------------------------------------------
    # Return number of states
    def num_states
        @state.size
    end

    #---------------------------------------------------------------
    # Creates a new state 
    def new_state
        newID = @@nextID
        @@nextID += 1
        @state[newID] = true
        @transition[newID] = {}
        newID 
    end

    #---------------------------------------------------------------
    # Creates a new state
    def add_state(v)
        unless has_state?(v)
            @state[v] = true
            @transition[v] = {}
        end
    end

    #---------------------------------------------------------------
    # Returns true if the state exists
    def has_state?(v)
        @state[v]
    end

    #---------------------------------------------------------------
    # Set (or reset) the start state
    def set_start(v)
        add_state(v)
        @start = v
    end

    #---------------------------------------------------------------
    # Set (or reset) a final state
    def set_final(v, final = true)
        add_state(v)
        if final then
            @final[v] = true
        else
            @final.delete(v)
        end
    end

    #---------------------------------------------------------------
    # Returns true if the state is final
    def is_final?(v)
        @final[v]
    end

    #---------------------------------------------------------------
    # Creates a new transition from v1 to v2 with symbol x
    # Any previous transition from v1 with symbol x is removed
    def add_transition(v1, v2, x)
        add_state(v1)
        add_state(v2)
		
		if @transition[v1][x].nil? then
			@transition[v1][x] = [v2]
		else
			@transition[v1][x].push(v2)
		end
		
    end

    #---------------------------------------------------------------
    # Get the destination state from v1 with symbol x
    # Returns nil if non-existent
    def get_transition(v1,x)
        if has_state?(v1)
            @transition[v1][x]
        else
            nil
        end
    end

    #---------------------------------------------------------------
    # Returns true if the dfa accepts the given string
    def accept?(s, current = @start)
        if s == ""
            is_final?(current)
        else
            dest = get_transition(current,s[0,1])
            if dest == nil
                false
            else
                accept?(s[1..-1], dest)
            end
        end
    end

    #---------------------------------------------------------------
    # Prints FA 
    def pretty_print
        print "% Start "
	puts @start

        # Final states in sorted order
	print "% Final {"
	@final.keys.sort.each { |x| print " #{x}" }
	puts " }" 

        # States in sorted order
	print "% States {" 
	@state.keys.sort.each { |x| print " #{x}" }
	puts " }" 

        # Alphabet in alphabetical order
        print "% Alphabet {"
	@alphabet.sort.each { |x| print " #{x}" }
	puts " }" 

        # Transitions in lexicographic order
        puts "% Transitions {"
	@transition.keys.sort.each { |v1| 
            @transition[v1].keys.sort.each { |x| 
                v2 = get_transition(v1,x)
				v2.each{ |y|
                puts "%  (#{v1} #{x} #{y})" 
            }
			}
        }
	puts "% }" 
		
    end
        
    #---------------------------------------------------------------
    # Prints FA statistics
    def print_stats
        tmp = 0 
        puts "FiniteAutomaton"
        puts "  #{state.size} states"
        puts "  #{final.size} final states" 
		@transition.each_key{|x|
			@transition[x].each_key{ |y|
			@transition[x][y].each{ |z|
			tmp = tmp + 1
			}
			}
		}
		puts "  #{tmp} transitions"
		
		shash = Hash.new(0)
        
		counter0 = 0
		@transition.each_key{ |x|
				if @transition[x].size == 0 then
					counter0 +=1
				end
		}
		
		if counter0 > 0 then
		puts "    #{counter0} states with 0 transitions"
		
		end
		
		
		@transition.each_key{ |x|
			counter1 = 0
			
			@transition[x].each_key{ |y|
				counter1 += @transition[x][y].size
			}
			var = shash[counter1] 
			var = var + 1
			shash[counter1] = var
		}
		
		shash.keys.sort.each{ |e|
			if !(e == 0) then
			puts "    #{shash[e]} states with #{e} transitions"
			end
		
		}
    end

    #---------------------------------------------------------------
    # accepts just symbol ("" = epsilon)
    def symbol! sym
        initialize
        s0 = new_state
        s1 = new_state
        set_start(s0)
        set_final(s1, true)
        add_transition(s0, s1, sym)
        if (sym != "") && (!@alphabet.include? sym)
            @alphabet.push sym
        end
    end

    #---------------------------------------------------------------
    # accept strings accepted by self, followed by strings accepted by newFA
    def concat! newFA
	
	
	@final.each_key{ |k|
		add_transition(k, newFA.start, "")
	}
	
	#updating transitions
	@transition.update(newFA.transition)
	@state.update(newFA.state)
	
	#adding alphabet
	@alphabet = @alphabet|newFA.alphabet
	@alphabet.sort!
	
	#delete final of current dfa
	@final.each_key{ |x|
		set_final(x, false)
	}
	
	#setting final state
	newFA.final.each_key{ |x|
		set_final(x, true)
	}
		
	
    end

    #---------------------------------------------------------------
    # accept strings accepted by either self or newFA
    def union! newFA
	
	#putting in newFA transitions
	@transition.update(newFA.transition)
	@state.update(newFA.state)
	@alphabet = @alphabet|newFA.alphabet
	
	#newFA.transition.each_pair{ |k,v|
	#add_state(k)
	#	@tansition[k] = v
	#}
	#creating a new start state
	newStart = new_state
	
	#putting in transition from new start state to old ones
	add_transition(newStart, @start, "")
	add_transition(newStart, newFA.start, "")
	
	#addin in final states of newFA
	newFA.final.each_key{ |x|
		set_final(x, true)
	}
	
	#creating a new final state
	newFinal = new_state
	
	#transition from old finals to new final 
	@final.each_key{ |k|
		add_transition(k, newFinal, "")
	}
	
	@final.each_key{ |k|
		set_final(k, false)
	}
	
	#setting new final
	set_final(newFinal, true)
	
	#setting new start
	set_start(newStart)
	
    end

    #---------------------------------------------------------------
    # accept any sequence of 0 or more strings accepted by self
    def closure! 
	
	#new start state
	newStart = new_state
	
	#adding new start to old start transition
	add_transition(newStart, @start, "")
	
	#setting new start
	set_start(newStart)
	
	#creating a new final state
	newFinal = new_state
	
	#adding transition from old final to new final
	@final.each_key{ |x|
	add_transition(x, newFinal, "")
	}
	
	@final.each_key{ |x|
		set_final(x, false)
	}
	
	set_final(newFinal, true)
	
	@final.each_key{ |x|
	add_transition( x, newStart, "")
	add_transition(newStart, x, "")
	}
	
    end

		def belongs_equal (listOne, listTwo)
		#returns true is listOne belongs equal to listTwo
		listOne = listOne.sort
		listTwo = listTwo.sort
		
		listOne.each { |l1|
			if (listTwo.include?(l1) == false)
				return false
			end
		}
		return true
	end
    	def get_empty_states v
		#BaseCaseCondition
		empty_states = Array.new
		@transition.keys.each{ |v1|
			if (v1 == v)
				transition[v1].keys.each{ |x|
					if (x == "")
						transition[v1][x].each{ |v2|
							empty_states.push(v2)
						}
					end
				}
			end
		}
		
		return empty_states
	end
		def eClosure (cList,visited)
		if belongs_equal(cList,visited)
			return visited
		end
		res = Array.new
		cList.each{ |x|
				if visited.include?(x)
					next
				end
				emp = Array.new
				emp = get_empty_states(x)
				emp.each{ |e|
					res.push(e)
				}
			}
		return eClosure(cList|res, cList)
	end
	def ec (visit, states)
		visit = visit.sort
		states = states.sort
		flag = true
		states.each { |s|
			if (!visit.include?(s))
				flag = false
			end
		}
		if (!flag)
			l = []
			states.each{ |t|
					if (!visit.include?(t))
						un = []
						@transition.keys.each{ |r|
							if (r == t)
								transition[r].keys.each{ |a|
									if (a == "")
										transition[r][a].each{ |f|
											un.push(f)
										}
									end
								}
							end
						}

						un.each{ |o|
							l.push(o)
						}
					end
				}
			return ec(states, l|states)
		else
			visit = visit.sort
			return visit
		end
	end
    #---------------------------------------------------------------
    # returns DFA that accepts only strings accepted by self 
	def to_dfa
        # create a new one, or modify the current one in place,
        # and return it
		#NFA( alphabet, set of states, start state, set of final states, transitions)
		#DFA(alphabet, set of states, start state, set of final states, transitions)
		
        d = FiniteAutomaton.new
		r0 = ec([],[].push(@start)) #r0 has array from ec
		
		hash_R = Hash.new
		hash_visit = Hash.new
		
		s = d.new_state
		d.set_start(s)
		
		hash_R[r0] = s
		t = []
		
		r2 = [].push(r0)  #array with r0
		
		while (!r2.empty?) do
			
			r = r2.pop
			
			if (!hash_visit[r])
				x = hash_R[r]
				hash_visit[r] = true
				
				@alphabet.sort.each {|a|
					moved = Array.new
					
					
					r.sort.each{ |eachs|
						s2 = (get_transition(eachs,a)) # { {1} => {a, 2,3,4}} s can be 2 3 4
						if s2
							moved = moved|s2
						end
					}
					moved.sort!
					arr = Array.new
					moved.sort.each{ |r3| arr.push(r3) }
					
					if (!arr.empty?)
						
						t = ec([],arr)
						z = hash_R[t]
						
						unless z
							z = d.new_state
							hash_R[t] = z
							r2.push(t)
						end
					
						if (z)
							d.add_transition(x,z,a)
							if !(d.alphabet.include?(a)) then
								d.alphabet.push(a)
							end
						end
					end
						
				}
			end			
		end

		hash_R.keys.sort.each { |k|
			k.each { |y|
				if (is_final?(y))
					d.set_final(hash_R[k])
				end
				}
			}
		
		return d
	end

	
	#e-closure 
	def ec6 state
	
	states_array = []
	
	value = @transition[state][""]
	
	if( value.nil?) then
		return states_array
	end
	
	value.each{ |s| 
		states_array = states_array|ec(s)
	}
	
	states_array = (states_array | [].push(state))
	return states_array.sort!
	
	end

    #---------------------------------------------------------------
    # returns a DFA that accepts only strings not accepted by self, 
    # and rejects all strings previously accepted by self
    def complement!
        # create a new one, or modify the current one in place,
        # and return it
		
		#for each state get a transition on it..if it doesnt have a transition
		#then add a transition of both alphabets to dead state
		
	dead_state = new_state
	
		count = 0
		@state.keys.each{ |s|
			@alphabet.each{ |a|
			trans = get_transition(s, a)
			if trans.nil? then
				count += 1
			end
			puts "#{count} count"
			puts "#{alphabet.size} alphabet size"
			}
			
			if count == alphabet.size then
			@alphabet.each{ |b|
				add_transition(s, dead_state, b)
				
				}
			end
			
			count = 0
		}
		
		
		@state.keys.each { |s|
			@final.keys.each { |f|
				if s == f then
					set_final(s, false)
				else
					set_final(s, true)
				end
				}
			}
		
    end

    #---------------------------------------------------------------
    # return all strings accepted by FA with length <= strLen
    def gen_str strLen
	sortedAlphabet = @alphabet.sort
        resultStrs = [ ] 
        testStrings = [ ]
        testStrings[0] = [] 
        testStrings[0].push ""
        1.upto(strLen.to_i) { |x|
            testStrings[x] = []
            testStrings[x-1].each { |s|
                sortedAlphabet.each { |c|
                    testStrings[x].push s+c
                }
            }
        }
        testStrings.flatten.each { |s|
            resultStrs.push s if accept? s
        }
        result = ""
        resultStrs.each { |x| result.concat '"'+x+'" ' }
        result
    end

end

#---------------------------------------------------------------
# read standard input and interpret as a stack machine

def interpreter file
   dfaStack = [ ] 
   loop do
       line = file.gets
       if line == nil then break end
       words = line.scan(/\S+/)
       words.each{ |word|
           case word
               when /DONE/
                   return
               when /SIZE/
                   f = dfaStack.last
                   puts f.num_states
               when /PRINT/
                   f = dfaStack.last
                   f.pretty_print
               when /STAT/
                   f = dfaStack.last
                   f.print_stats
               when /DFA/
                   f = dfaStack.pop
                   f2 = f.to_dfa
                   dfaStack.push f2
               when /COMPLEMENT/
                   f = dfaStack.pop
                   f2 = f.complement!
                   dfaStack.push f2
               when /GENSTR([0-9]+)/
                   f = dfaStack.last
                   puts f.gen_str($1)
               when /"([a-z]*)"/
                   f = dfaStack.last
                   str = $1
                   if f.accept?(str)
                       puts "Accept #{str}"
                   else
                       puts "Reject #{str}"
                   end
               when /([a-zE])/
                   puts "Illegal syntax for: #{word}" if word.length != 1
                   f = FiniteAutomaton.new
                   sym = $1
                   sym="" if $1=="E"
                   f.symbol!(sym)
                   dfaStack.push f
               when /\*/
                   puts "Illegal syntax for: #{word}" if word.length != 1
                   f = dfaStack.pop
                   f.closure!
                   dfaStack.push f
               when /\|/
                   puts "Illegal syntax for: #{word}" if word.length != 1
                   f1 = dfaStack.pop
                   f2 = dfaStack.pop
                   f2.union!(f1)
                   dfaStack.push f2
               when /\./
                   puts "Illegal syntax for: #{word}" if word.length != 1
                   f1 = dfaStack.pop
                   f2 = dfaStack.pop
                   f2.concat!(f1)
                   dfaStack.push f2
               else
                   puts "Ignoring #{word}"
           end
        }
   end
end

#---------------------------------------------------------------
#main( )

if false			# just debugging messages
    f = FiniteAutomaton.new
    f.set_start(1)
    f.set_final(2)
    f.set_final(3)
    f.add_transition(1,2,"a")   # need to keep this for NFA
    f.add_transition(1,3,"a")  
    f.prettyPrint
end

if ARGV.length > 0 then
  file = open(ARGV[0])
else
  file = STDIN
end

interpreter file  # type "DONE" to exit