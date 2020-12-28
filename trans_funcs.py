'''
A script to create translation functions from the fsts saved in /lib/
'''

from pynini import *
import csv

################
## Alg to Eng ##
################

def A(s: str) -> Fst:
    return acceptor(s, token_type="utf8")
    
def list_string_set(acceptor):
    my_list = []
    paths = acceptor.paths()
    for s in paths.ostrings():
        my_list.append(s)
    my_list.sort(key=len)
    return my_list

path_to_lib = "/home/ekp/Documents/SBU_Fall2020/Thesis/Thesis_code/LIA_FST/lib/"
alg2eng_machine = Fst.read(path_to_lib+'alg2eng.fst')

def alg2eng(alg):
    alg = A(alg)
    eng = alg @ alg2eng_machine
    return(list_string_set(eng))
    
eng2alg_machine = Fst.read(path_to_lib+'eng2alg.fst')
    
def eng2alg(eng):
    eng = A(eng)
    alg = eng @ eng2alg_machine
    return(list_string_set(alg))
    
print('Launching GUI')
