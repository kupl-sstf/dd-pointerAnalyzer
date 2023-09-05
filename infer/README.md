# Pointer analysis library for Infer

## Running
Using makefile:
```
make clean
infer capture -- make
infer analyze --pointer-call-context-depth 3 --pointer-heap-context-depth 2 --pointer-max-access-path 6 --pointer-apply-tunneling
```
Analyzing a single file:
```
infer analyze -- clang simple1.c 
```

