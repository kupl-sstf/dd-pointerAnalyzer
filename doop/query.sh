#!/bin/bash
 
  bloxbatch -db last-analysis -query CandidateHeap2obj | sort > CandidateHeap2obj.facts
  python process.py CandidateHeap2obj.facts > CanHeap2obj.facts

  bloxbatch -db last-analysis -query CandidateHeap2type | sort > CandidateHeap2type.facts
  python process.py CandidateHeap2type.facts > CanHeap2type.facts
 
  bloxbatch -db last-analysis -query CandidateHeap1type | sort > CandidateHeap1type.facts
  python process.py CandidateHeap1type.facts > CanHeap1type.facts
  
