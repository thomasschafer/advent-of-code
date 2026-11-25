P players and N marbles

Every 23rd marble is kept and added as points, plus the marble 7 indices behind current is removed and added, and that index becomes current

We could just build the list of marbles manually but this will be slow and I assume part 2 will use huge numbers which makes this intractable, especially given that 23 and 7 are prime

Iterate k from 1 to N//23
Player capturing point at that step 23*k is the 23*k mod P th player
They capture marble 23*k plus the marble 7 behind 23*k-1th, how do we determine the value of this?
- At each 23*k-1th step there will be 23*k-1-(k-1)*2 marbles
- Next marble placed 2 indices ahead, all  later elements pushed forward, it becomes current
- We need to exclude marbles that will have been removed
