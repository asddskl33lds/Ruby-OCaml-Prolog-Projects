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
		
		#Part I below
		
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
	
	def add_alpha(x)
		@alphabet = @alphabet.push(x)
	end

    #---------------------------------------------------------------
    # Set (or reset) a final state
    def set_final(v, final = true)
        add_state(v)
        if final
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
		if (@transition[v1][x] == nil)
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
		
		#Return false if it has a epsilon transition or it has more than one state with same transition alphabet
		@transition.keys.sort.each { |v1| 
            @transition[v1].keys.sort.each { |x|
				if (x == "")
					return false
				end
				if (@transition[v1][x].length > 1)
					return false
				end
			}
		}
	
	
        if s == ""
            is_final?(current)
        else
            dest = get_transition(current,s[0,1])
            if dest == nil
                false
            else
                accept?(s[1..-1], dest[0])
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
	#puts @alphabet
	@alphabet.sort.each { |x| print " #{x}" }
	puts " }" 

        # Transitions in lexicographic order
        puts "% Transitions {"
	@transition.keys.sort.each { |v1| 
            @transition[v1].keys.sort.each { |x|
				@transition[v1][x].each { |v2|
					puts "%  (#{v1} #{x} #{v2})" 
				}
            }
        }
	puts "% }" 
    end
        
    #---------------------------------------------------------------
    # Prints FA statistics
    def print_stats
        puts "FiniteAutomaton"
		temp = @state.length
        puts "  #{temp} states"
		temp = @final.length
        puts "  #{temp} final states" 
		tmp = 0
		@transition.keys.sort.each { |v1| 
            @transition[v1].keys.sort.each { |x|
				@transition[v1][x].each { |v2|
					tmp = tmp + 1
				}
            }
        }
        puts "  #{tmp} transitions"
		
		
		trans = Hash.new(0)
		@transition.keys.each { |v1| 
			len = 0
			@transition[v1].keys.each{ |x|
				len += @transition[v1][x].length
			}
			trans[len] += 1
        }
		numOfStatesWithTransition = 0
		trans.keys.sort.each{ |k|
	
			numOfStatesWithTransition += trans[k]
		}
		numWithoutTransition = @state.size - numOfStatesWithTransition
		if (numWithoutTransition > 0)
			puts "    #{numWithoutTransition} states with 0 transitions"
		end
		trans.keys.sort.each{ |k|
		    puts "    #{trans[k]} states with #{k} transitions" 
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
	
		@state.update(newFA.state)
		
		@alphabet = @alphabet | newFA.alphabet
		@final.keys.each { |x|
			add_transition(x, newFA.start, "")
		}
		@final.clear
		newFA.final.keys.each{ |v|
			set_final(v)
		}
		
		#go through all keys - alphabets->one more
		@transition.update(newFA.transition)		
    end

    #---------------------------------------------------------------
    # accept strings accepted by either self or newFA
    def union! newFA
		s1 = new_state
		s2 = new_state
		@state.update(newFA.state)
		@transition.update(newFA.transition)
		@alphabet = @alphabet | newFA.alphabet
		add_transition(s1, @start, "")
		add_transition(s1, newFA.start, "")
		
		@final.keys.each{ |x|
			add_transition(x, s2, "")
		}
		@final.clear
		newFA.final.keys.each{ |x|
			add_transition(x, s2, "")
		}
		@start = s1;	
		set_final(s2)
    end

    #---------------------------------------------------------------
    # accept any sequence of 0 or more strings accepted by self
    def closure! 
		s1 = new_state
		s2 = new_state
		
		add_transition(s2, s1, "")
		add_transition(s1, s2, "")
		
		add_transition(s1, @start, "")
		@final.keys.each{ |x|
			add_transition(x, s2, "")
		}
		@start = s1
		@final.clear
		set_final(s2)
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
	
	def ec state
		
		states_array = []
		
		value = @transition[state][""]
		states_array.push(state)
		
		if( value.nil?) then
			return states_array
		end
		
		value.each{ |s| 
			states_array = states_array|ec(s)
		}
		
		
		return states_array.sort!
	
	end
	
	def to_dfa
        # create a new one, or modify the current one in place,
        # and return it
		#NFA( alphabet, set of states, start state, set of final states, transitions)
		#DFA(alphabet, set of states, start state, set of final states, transitions)
		
        d = FiniteAutomaton.new
		r0 = eClosure([].push(@start),[]) #r0 has array from ec
		
		hash_R = Hash.new
		hash_visit = Hash.new
		
		s = d.new_state
		d.set_start(s)
		
		hash_R[r0] = s
		t = []
		
		r2 = [].push(r0)  #array with r0
		
		while (!r2.empty?) do
			#current_state = hash_N[r]
			#puts " current_state #{current_state}"
			
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
						r = []
						arr.each { |array_element|
							r = r|eClosure([].push(array_element),[])
						}
						t = r
						z = hash_R[t]
						
						unless z
							z = d.new_state
							hash_R[t] = z
							r2.push(t)
						end
					
						if (z)
							d.add_transition(x,z,a)
							
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

    #---------------------------------------------------------------
    # returns a DFA that accepts only strings not accepted by self, 
    # and rejects all strings previously accepted by self
    def complement!
        # create a new one, or modify the current one in place,
        # and return it
        
		#FiniteAutomaton.new 

		#Lets calculate num of States with zero trans
		numDead = 0
		@state.keys.each{ |v1|
			@alphabet.each {|x|
				if (@transition[v1][x] == nil)
					numDead += 1
				end
			}
		}
		
		#Lets make states point to that Dead State
		if (numDead > 0)
			deadState = new_state
			@alphabet.each { |x|
				add_transition(deadState, deadState, x)
				}
			# Now we have a dead state here!!
			@state.keys.each{ |v1|
				@alphabet.each{ |x|
					if (@transition[v1][x] == nil)
						add_transition(v1, deadState, x)
					end
				}
			}
		end
		
		#Finals are Non-Finals! <-> Non-Finals are Finals
		@state.keys.each {|v1| 
			set_final(v1, !is_final?(v1))
		}
		
		return self
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
# main( )

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

