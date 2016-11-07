
# Markov Decision in Tonal Music

David Benoit and Brian Chiang

## Problem Statement
This project will explore the problem domain of the generation tonal music.  In particular, it will focus on generating music which follows the harmonic and contrapuntal rules developed by the music theorist Heinrich Schenker.

It is interesting because such a system can "[make] choices related to style, harmony,rhythm or any musical attribute." [1].  This project will employ Markov Decision Processes, because they have a history of use in computational music models, are easy to adapt to different styles of music[2]. We will also explore the use of context-free grammars in the generation of musical excerpts, as context-free grammar algorithms have been proposed as likely to be successful in generating music[3, 4].


## Problem Analysis

### State Space Formulation

The total state space of a musical exerpt is comprised of the number of voices (v), the range of each voice (r), and the total possible beat values for each note (b).  The approximate size of all possible states is therefore O(vbr).  

Because of the consistent structure of tonal music, there are optimizations available to significantly shrink the state space in order to improve the performance of a generative algorithm.  The most important optimization will procede with the idea that tonal harmony is equivalent to a context-free language[3, 4].  As such, the harmonic structure of the piece will be generated beforehand using a context-free grammar based generative algorithm.  This will shrink the state space to only be concerned with the notes in the given harmony of the state.  Therefore the final state space this program will be concerned with is the number of voices (v), the available notes in the current harmony of each voice (h < r), and the total possible beat values for each note (b), or O(vbh).

Each state will be represented as a set of notes, and the given harmony. 


### State Transition Function

This project will employ a multi-agent algorithm, generating voices in sequence.  Each voice will have its own agent which, based on the predetermined harmonic structure and previous states, will calculate the optimal note.  Each agent will take over in the state tree at the state where the previous agent finished, and will therefore be able to see all of the notes that were produced by the previous agents in order to incorporate them into the current state.  

### Evaluation Function

The system will have a function which checks whether the counterpoint is correct based on Schenkerian Analysis of the excerpt.  It will recognize idiomatic contrapuntal devices in the excerpt, and give a reward to based on the use of such devices.

### Other Problem Representation Paramaters

The problem is fully observeable, multi-agent, deterministic, sequential, and discrete.

## Data Set and Source Materials
We will be creating our own data set from a simulation we build.

## Deliverable and Demonstration
By the end of the project, we will have a system which, given a numeric seed and parameters such as length, will generate an excerpt of authentic sounding tonal music.

## Evaluation of results
We will know if we are successful if the excerpt successfully passes a Schenkerian Analysis.

## References
[1] Aengus Martin, Craig Jin, Andre van Schaik, William L. Martens. PARTIALLY OBSERVABLE MARKOV DECISION PROCESSES FOR INTERACTIVE MUSIC SYSTEMS (p. 490). Sydney University, NSW 2006, Australia (2016)

[2]  Arne Eigenfeldt and Philippe Pasquier. Realtime Generation of Harmonic Progressions Using Controlled Markov Selection (p. 2). Simon Fraser University, Canada

[3] Bradley J. Clement. Learning Harmonic Progression Using Markov Models (p. 11) http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.52.3015&rep=rep1&type=pdf

[4] Martin Rohrmeier. Towards a generative syntax of tonal harmony (p. 2). Journal of Mathematics and Music Vol. 5, No. 1, March 2011, 35–53
