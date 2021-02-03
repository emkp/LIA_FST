'''
Last updated on 3 February 2021
A script to build the following FST machines:
	gloss2analysis	analysis2gloss
	analysis2alg	alg2analysis
	gloss2alg	alg2gloss
	gloss2eng	eng2gloss
	eng2analysis	analysis2eng
	eng2alg	alg2eng
And to generate entries.csv, a csv file storing every available
Algonquian-English translation pair along with their associated
glosses and anaylzed forms.	
'''
from pynini import *
from datetime import datetime
import pandas as pd
import csv

############
# known bugs/ things to check
############
# tkinter won't print words with the special characters á or ô
# check third-person 'go' forms are correct
	# 'A NA goes.' == 'ôw'
# move #Getting the generated configurations# and #Getting attested forms from data# sections to new file
# question about the N central suffix's context:
	# V DN(SP) ?? or F DN(SP) going with F DN(SP) for now


#########
# setup #
#########

def A(s):
    return acceptor(s, token_type='utf8')

def T(upper, lower):
    return cross(A(upper),A(lower))

a = A('a')
zero = a-a
zero.optimize()

def newclass(letters):
    ''' returns (c1|c2|c3|...) '''
    temp = zero
    for x in letters: temp = A(x)|temp
    return temp.optimize()

bound = A('.')
del_bound = T('.','')
add_bound = T('','.')
space = A(' ')
del_space = T(' ','')
add_space = T('',' ')
plus = A('+')
add_plus = T('','+')
slash = A('/')

alg_vowel_chars = 'auáiôo'
alg_consonant_chars = 'ktpcwynmshrq'
alg_chars = alg_vowel_chars+alg_consonant_chars
eng_vowel_chars = 'aeiou'
eng_consonant_chars = 'bcdfghjklmnpqrstvwxyz'+'-_'
eng_capitals = 'IWYTA'
eng_chars = eng_vowel_chars+eng_consonant_chars
gloss_chars = 'SPDIAN12ETCRBOG.+/ '

alg_vowel = newclass(alg_vowel_chars)
alg_consonant = newclass(alg_consonant_chars)
alg_con_or_vow = alg_vowel|alg_consonant
eng_vowel = newclass(eng_vowel_chars)
eng_consonant = newclass(eng_consonant_chars)
gloss_class = newclass(gloss_chars)

other_morphemes = closure(newclass(alg_chars+'+'))
other_eng  = closure(newclass(eng_chars+' '))

sigmaStar = (closure(alg_vowel|alg_consonant|eng_vowel|eng_consonant|gloss_class)).optimize()
sigmaStar_w_capitals = (closure(alg_vowel|alg_consonant|eng_vowel|eng_consonant|gloss_class|newclass(eng_capitals)))

def complement(machine):
    return (difference(sigmaStar, machine)).optimize()

def has(machine):
    return (sigmaStar+machine+sigmaStar).optimize()

def not_has(machine):
    return (complement(has(machine))).optimize()

def has_two(machine):
    return (sigmaStar+machine+sigmaStar+machine+sigmaStar).optimize()

def has_both(first,second):
    first_first = sigmaStar+first+sigmaStar+second+sigmaStar
    second_first = sigmaStar+second+sigmaStar+first+sigmaStar
    return (first_first|second_first).optimize()
    
######################################
## shortcut transducers: Algonquian ##
######################################

E = A('E')       # E empty
one = A('1')     # 1 first person
two = A('2')     # 2 second person
S = A('S')       # S singular
P = A('P')       # P plural
D = A('D')       # D definite
I = A('I')       # I indefinite
An = A('A')      # A animate 3rd person
N = A('N')       # N inanimate 3rd person

oneS = one+S     # 1st person singular
oneP = one+P     # 1st person plural exclusive
oneSP = (oneS|oneP).optimize() # 1st person singular or plural (not including we-inc)
twoS = two+S     # 2nd person singular
twoP = two+P     # 2nd person plural
twoSP = (twoS|twoP).optimize() # 2nd person singular or plural (also catches we-inc)
weinc = A('12P') # 1st person plural inclusive

ym = (oneSP|twoSP|weinc).optimize() # speech-act participants only

DA = D+An # definite animate 3rd person
DN = D+N  # definite inanimate 3rd person
IA = I+An # indefinite animate 3rd person
IN = I+N # indefinite inanimate 3rd person

DAS = DA+S          # definite animate singular
DAP = DA+P          # definite animate plural
DASP = (DAS|DAP).optimize() # definite animate singular or plural
DNS = DN+S          # definite inanimate singular
DNP = DN+P          # definite inanimate plural
DNSP = (DNS|DNP).optimize() # definite inanimate singular or plural
IAS = IA+S          # indefinite animate singular
IAP = IA+P          # indefinite animate plural
IASP = (IAS|IAP).optimize() # indefinite animate singular or plural
INS = IN+S          # indefinite inanimate singular
INP = IN+P          # indefinite inanimate plural
INSP = (INS|INP).optimize() # indefinite inanimate singular or plural

threeS = (DAS|DNS|IAS|INS).optimize()
threeP = (DAP|DNP|IAP|INP).optimize()
threeSP = (threeS|threeP).optimize()

NI = (DNSP|INSP).optimize()
NA = (DASP|IASP).optimize()

V = (ym|DASP|DNSP).optimize() # visible arguments; speech act participants and definite third-person
Ag = (ym|NA).optimize() # "active"/agentive visible arguments; speech act participants and definite animate third-person
V3 = (DASP|DNSP).optimize() # visible 3rd-person arguments; definite third-person (animate and inanimate)

X = (IASP|INSP|E).optimize() # invisible
X3 = (IASP|INSP).optimize() # invisible third person; i.e. indefinite third person

F = (oneSP|twoSP|weinc|DASP|DNSP|IASP|INSP).optimize() # "filled"; any non-empty argument
FE = (F|E).optimize() # any argument including empty

reflexive = has_two(one)|has_two(two)

positive = A('O')
negative = A('G')
polarity = positive|negative

any_config = (((F+bound+F+bound+FE)|(F+bound+E+bound+E))+slash+polarity) @ complement(reflexive)

#####################################
### shortcut transducers: English ###
#####################################

eng_I = A('i')
eng_we = A('we')
eng_weinc = A('we-inc')
eng_you = A('you')
eng_yall = A('yall')
eng_that_na = A('that na')
eng_those_nas = A('those nas')
eng_that_ni = A('that ni')
eng_those_nis = A('those nis')
eng_a_na = A('a na')
eng_some_nas = A('some nas')
eng_a_ni = A('a ni')
eng_some_nis = A('some nis')
are_subjects = (eng_you|eng_yall|eng_we|eng_weinc|eng_those_nas|eng_some_nas).optimize()
threeSg_subjects = (eng_that_na|eng_a_na)
possible_subjects = (eng_I|eng_we|eng_weinc|eng_you|eng_yall|
                     eng_that_na|eng_those_nas|eng_that_ni|eng_those_nis|
                     eng_a_na|eng_some_nas|eng_a_ni|eng_some_nis).optimize()

###########################
##### Build gloss2alg #####
###########################
print('\nBuilding gloss2alg machine')
print('----------------------------')

#######
# stems
#######
    
class Word:
    '''
    Class to store a stem from glossary.txt and its information
    
    Attributes
    ----------
    eng_stem        the English stem
    transitivity    intransitive, monotransitive, ditransitive
    alg_TA_stem     the TA stem
    alg_TI_stem     the TI stem
    alg_AI_stem     the AI stem
    alg_II_stem     the II stem
    regular         regular or irregular, referring to stem-internal sound changes
    forms           a list containing the TA, TI, AI, II stems, in that order
    num_forms       the number of forms available for this word
    
    Methods
    ------
    summary         prints a message about the word's transitivity, regularity, and Eng and Alg forms,
                    for checking whether they were read in correctly
    '''
    def __init__(self, eng_stem, transitivity, TA=None, 
                 TI=None, AI=None, II=None, regular_spelling=True, regular_order=True):
        self.eng_stem = eng_stem
        self.transitivity = transitivity
        self.TA = TA
        self.TI = TI
        self.AI = AI
        self.II = II
        self.regular_spelling = regular_spelling
        self.regular_order = regular_order
        self.forms = [self.TA, self.TI, self.AI, self.II]
        self.num_forms = len([f for f in self.forms if f!=None])
        
    
    def summary(self):
        print("eng stem:", self.eng_stem)
        print("transitivity:", self.transitivity)
        print("number of forms:", self.num_forms)
        if self.TA:
            print("TA", self.TA)
        if self.TI:
            print("TI", self.TI)
        if self.AI:
            print("AI", self.AI)
        if self.II:
            print("II", self.II)
        print('regular_spelling:', self.regular_spelling)
        print('regular_order:', self.regular_order)
        print()

with open('./glossary.csv', newline='') as csvfile:
    glossaryreader = csv.reader(csvfile, delimiter=',')
    glossary_rows = [r for r in glossaryreader]

glossary_words = []
for row in glossary_rows[1:]:
    eng_stem = row[0]
    transitivity = row[1]
    TA = row[2] if row[2]!='-' else None
    TI = row[3] if row[3]!='-' else None
    AI = row[4] if row[4]!='-' else None
    II = row[5] if row[5]!='-' else None
    regular_spelling = False if row[6]=='irregular' else True
    regular_order = False if row[7]=='irregular' else True
    glossary_words.append(Word(eng_stem,transitivity,TA,TI,AI,II,
                               regular_spelling,regular_order))
    
def build_regular_stems(glossary_words):
    TA_ditrans_context = (Ag+bound+Ag+bound+F) @ complement(reflexive)
    TA_monotrans_context = (Ag+bound+Ag+bound+E) @ complement(reflexive)
    TI_context = Ag+bound+NI+bound+E
    AI_context = Ag+bound+E+bound+E
    II_context = NI+bound+E+bound+E
    
    temp = zero
    for word in glossary_words:
        if word.regular_spelling:

            stem_gloss = []
            if word.TA:
                stem = T(word.eng_stem,word.TA)
                if word.transitivity=='ditrans':
                    stem_gloss.append(stem+del_bound+add_plus+TA_ditrans_context+slash+polarity)
                if word.transitivity=='monotrans':
                    stem_gloss.append(stem+del_bound+add_plus+TA_monotrans_context+slash+polarity)
            if word.TI:
                stem = T(word.eng_stem,word.TI)
                stem_gloss.append(stem+del_bound+add_plus+TI_context+slash+polarity)
            if word.AI:
                stem = T(word.eng_stem,word.AI)
                stem_gloss.append(stem+del_bound+add_plus+AI_context+slash+polarity)
            if word.II:
                stem = T(word.eng_stem,word.II)
                stem_gloss.append(stem+del_bound+add_plus+II_context+slash+polarity)

            for sg in stem_gloss:
                temp = temp|sg
    return temp

regular_stems = build_regular_stems(glossary_words)

# irregular stems
mis_context = (twoSP+bound+oneSP+bound+F) # implicitly rules out reflexives
mir_context = complement(mis_context) @ (Ag+bound+Ag+bound+F) @ complement(reflexive)
mis = T('give','mis')+del_bound+add_plus+mis_context
mir = T('give','mir')+del_bound+add_plus+mir_context
mis_mir = ((mis|mir)+slash+polarity).optimize()
irreg_stems = mis_mir

stems = regular_stems|irreg_stems

### functions for building morpheme transducers
def build_context_transducer(sub,prim_o,sec_o,polar=polarity,to_suf='',separate=plus,lang='alg'):
    temp = sub+bound+prim_o+bound+sec_o+slash+polar+other_morphemes
    if to_suf != '':
        temp = temp + add_plus + T('',to_suf)
    return temp.optimize()

def build_morpheme_transducer(to_union):
    middle = zero
    for machine in to_union:
        middle = middle|machine
    return (other_morphemes+plus+middle).optimize()
    
#############
# theme signs
#############

# add -u if 2.1.FE
themesign_u = build_context_transducer((twoSP|weinc),oneSP,FE,to_suf='u')

# add -ur if 1.2.FE
themesign_ur = build_context_transducer(oneSP,(twoSP|weinc),FE,to_suf='ur')

# add -ô if F.3.FE
themesign_o = build_context_transducer(F,NA,FE,to_suf='ô')

# add -uko if 3.F.FE and not F.3.FE
themesign_uko = build_context_transducer(threeSP,(F-threeSP),FE,to_suf='uko')

# add no theme sign if F.E.E or F.N.E
themesign_none = build_context_transducer(F,(NI|E),E,)

theme_sign = build_morpheme_transducer([themesign_u,themesign_ur,themesign_o,
                                        themesign_uko,themesign_none])

##########
# negation
##########

negative_config = build_context_transducer(F,FE,FE,negative)

# add -ow if the last letter is a consonant
algneg_ow_context = negative_config @ (sigmaStar+alg_consonant)
algneg_ow = algneg_ow_context+T('','+ow')

# add -w if the last letter is a vowel
algneg_w_context = (negative_config@complement(algneg_ow_context))
algneg_w = algneg_w_context+T('','+w')

# add nothing if the word is positive
algneg_none = build_context_transducer(F,FE,FE,positive)

alg_negation = build_morpheme_transducer([algneg_ow,algneg_w,algneg_none])

################
# central suffix
################

# N if:
    # there are three visible args                        # V.V.V
    # OR it's monotransitive with a def NI primary object # F.DNSP.E
    ## note that F.DNSP.X3 is not a legal configuration, so # F.DNSP.E is sufficient
    ## !! open question: is IASP.DNSP.E still an N? !! for now, yes
    
centsuf_N1 = build_context_transducer(V,V,V,)
centsuf_N2 = build_context_transducer(F,DNSP,E)
centsuf_N = (centsuf_N1|centsuf_N2).optimize()+T('','+ná')

# W if it doesn't fit the N conditions
    # AND there is one visible and one invisible          # (V X)|(X V)
    # OR if all arguments are invisible                   # X.X.X
centsuf_W1 = ((any_config-(centsuf_N1|centsuf_N2)) @ has_both(V3,X))+other_morphemes
centsuf_W2 = build_context_transducer(X,X,X)
centsuf_W = (centsuf_W1|centsuf_W2).optimize()+T('','+w')
    
# M if the only visible args are speech act participants  # ym.(X|ym).X or X.ym.X
    # AND there is at least one speech act participant    
centsuf_M = (any_config @ complement(has(V3)) @ has(ym))+other_morphemes+T('','+m')

centsuf = build_morpheme_transducer([centsuf_N,centsuf_W,centsuf_M])

########
# prefix
########

# may come back to this to figure out how to build prefixing into build_context_transducer()

# add ku- if there's a 2nd-person argument (including weinc)
prefix_ku1 = build_context_transducer((twoSP|weinc),FE,FE)
prefix_ku2 = build_context_transducer(F,(twoSP|weinc),FE)
prefix_ku = T('','ku+')+other_morphemes+plus+(prefix_ku1|prefix_ku2).optimize()


# add nu- if there's no 2nd-person but there is a 1st-person argument
prefix_nu1 = build_context_transducer(oneSP,FE,FE) @ complement(prefix_ku2)
prefix_nu2 = build_context_transducer(F,oneSP,FE) @ complement(prefix_ku1)
prefix_nu = T('','nu+')+other_morphemes+plus+(prefix_nu1|prefix_nu2).optimize()

# add wu- if there's no 2nd- or 1st-person argument but there is a 3rd-person; and the stem doesn't start with 'a'
prefix_wu = T('','wu+')+(alg_con_or_vow-(A('a')|A('ô')|A('w')))+other_morphemes+plus+build_context_transducer(threeSP,(threeSP|E),FE)

# add none if wu but stem starts with 'a'
prefix_none = (A('a')|A('ô')|A('w'))+other_morphemes+plus+build_context_transducer(threeSP,(threeSP|E),FE)

prefix = (prefix_ku|prefix_nu|prefix_wu|prefix_none).optimize()

######################
# personal pluralizers
######################

# add -nôn if there's a plural first person (including we-inc)
pp_non1 = build_context_transducer((oneP|weinc),FE,FE)
pp_non2 = build_context_transducer(F,(oneP|weinc),FE)
pp_non = (pp_non1|pp_non2).optimize() + T('','+nôn')

# add -wôw if there's a plural second or third person but not a plural second person; and not 3P.E.E
pp_wow_twoP_1 = build_context_transducer(twoP,FE,FE) @ complement(pp_non2)
pp_wow_twoP_2 = build_context_transducer(F,twoP,FE) @ complement(pp_non1)
pp_wow_twoP = (pp_wow_twoP_1|pp_wow_twoP_2).optimize()

pp_wow_threeP_1 = build_context_transducer(threeP,F,FE) @ complement(pp_non2)
pp_wow_threeP_2 = build_context_transducer(F,threeP,FE) @ complement(pp_non1)
pp_wow_threeP = (pp_wow_threeP_1|pp_wow_threeP_2).optimize()

pp_wow = (pp_wow_twoP|pp_wow_threeP)+T('','+wôw')

# add none if there's no plural or if the only plural is 3P.E.E
pp_none1 = any_config @ complement(has(P))+other_morphemes
pp_none2 = build_context_transducer(threeP,E,E)
pp_none = (pp_none1|pp_none2).optimize()

personal_plural = build_morpheme_transducer([pp_non,pp_wow,pp_none])

#############
# Peripherals
#############

###################
# object pluralizer

# add -ak if the outermost argument is a plural inanimate NA (!! open question: does it have to be definite?? !!)
op_ak_ditran = build_context_transducer(F,F,DAP)
op_ak_monotran = build_context_transducer(F,DAP,E)
op_ak_intran = build_context_transducer(DAP,E,E)
op_ak_conditions = (op_ak_ditran|op_ak_monotran|op_ak_intran).optimize()
op_ak = op_ak_conditions+T('','+ak')

# add -ash if the absolutive argument is a plural inanimate NI
op_ash_ditran = build_context_transducer(F,F,DNP)
op_ash_monotran = build_context_transducer(F,DNP,E)
op_ash_intran = build_context_transducer(DNP,E,E)
op_ash_conditions = (op_ash_ditran|op_ash_monotran|op_ash_intran).optimize()
op_ash = op_ash_conditions+T('','+ash')

# add none if not these conditions
op_none = (any_config @ complement(op_ak_conditions) @ complement(op_ash_conditions)) + other_morphemes

object_plural = build_morpheme_transducer([op_ak,op_ash,op_none])

##################
# obviative marker

# add -ah if there are two or more definite animate arguments
obviative_ah = other_morphemes+plus+(any_config @ has_two(DASP))+other_morphemes+T('','+ah')

# add none if there are not two or more definite animate arguments
obviative_none = other_morphemes+plus+(any_config @ complement(has_two(DASP)))+other_morphemes

#################
# peripherals = obviative and object plural; obviative takes precedence
peripherals = (obviative_ah | (obviative_none @ object_plural)).optimize()

##############
# remove gloss from final form; this includings reducing ++ to +
##############

to_remove = cross(any_config.project('input'),'')
reduce_plusses = cdrewrite(T('++','+'),sigmaStar,sigmaStar,sigmaStar)
remove_gloss = cdrewrite(cross(to_remove,''), plus, plus, sigmaStar) @ reduce_plusses

##############################
# phonology and spelling rules
##############################

# first remove plusses
remove_plusses = cdrewrite(T('+',''),sigmaStar,sigmaStar,sigmaStar)

###########
# insertion: intrusive t, u-bridge, i-bridge

# intrusive t: insert a 't' between 'u' and 'a'
intrusive_t = cdrewrite(T('','t'), A('u'), alg_vowel, sigmaStar)

# u-bridge: insert a 'u' in: mn, mw, wn, ww
u_bridge = cdrewrite(T('','u'),A('m')|A('w')|A('s')|A('r')|A('k'),A('n')|A('w')|A('m'),sigmaStar) # it might be more productive than this?...
    # not sure whether ww should be handled as wuw or w; see ku+mir+uko+w+wôw=kumirukowuw vs ku+mir+ô+w+wôw=kumirôw

# i-bridge: insert an 'i' in 
i_bridge = cdrewrite(T('','i'),A('c'),A('n'),sigmaStar)

insertion = (intrusive_t @ u_bridge @ i_bridge)

############
# truncation: no double a, delete final a, delete final ôn, delete final ôw, word-final uq, word final r->sh

# no double a
no_double_a = cdrewrite(T('áa','a'),sigmaStar,sigmaStar,sigmaStar)
no_doubles = no_double_a #@ no_double_w

# truncation processes: delete final a, onn, and onw
delete_final_a = cdrewrite(T("á",''),sigmaStar,'[EOS]',sigmaStar)
delete_final_um = cdrewrite(T("um",''),sigmaStar,'[EOS]',sigmaStar)

truncate_final_nonn = cdrewrite(T('ôn',''),'n','[EOS]',sigmaStar)
truncate_final_wonw = cdrewrite(T('ôw',''),'w','[EOS]',sigmaStar) # need to figure out when wonw and nonw truncation happens
truncate_final_w = cdrewrite(T('w',''),'t','[EOS]',sigmaStar)

truncation = (truncate_final_nonn @ truncate_final_wonw @ delete_final_a @ delete_final_um @ truncate_final_w).optimize()

# change 'uko+m' to 'uq'
kom_to_q = cdrewrite(T('ko+m','q'),sigmaStar,'[EOS]',sigmaStar)
truncate_uko = cdrewrite(T('uko','uq')|T('ko+m','q'),sigmaStar,'[EOS]',sigmaStar)

# word final r --> sh
word_final_r = cdrewrite(T('r','sh'),sigmaStar,'[EOS]',sigmaStar)

# put it all together
# I don't like how truncate_uko is its own thing here; is there a better way to do this?
phon_n_spell = (truncate_uko @ remove_plusses @ insertion @ no_doubles @ truncation @ word_final_r).optimize()


#############################
## Save gloss/alg machines ##
#############################
print('\nGenerating and saving gloss/alg/analysis machines')
print('-------------------------------------------------')

lib_path = './lib/'

# machines to translate between gloss and analysis
gloss2analysis_machine = (stems @ theme_sign @ alg_negation @ centsuf @ prefix @ personal_plural @ peripherals @ remove_gloss).optimize()
print('generated gloss2analysis_machine')
gloss2analysis_machine.write(lib_path+'gloss2analysis.fst')
print('saved gloss2analysis.fst')

analysis2gloss_machine = invert(gloss2analysis_machine).optimize()
print('generated analysis2gloss_machine')
analysis2gloss_machine.write(lib_path+'analysis2gloss.fst')
print('saved analysis2gloss.fst')

# machines to translate between analysis and algonquian
analysis2alg_machine = phon_n_spell
print('generated analysis2alg_machine')
analysis2alg_machine.write(lib_path+'analysis2alg.fst')
print('saved analysis2alg.fst')

alg2analysis_machine = invert(analysis2alg_machine).optimize()
print('generated alg2analysis_machine')
alg2analysis_machine.write(lib_path+'alg2analysis.fst')
print('saved alg2analysis.fst')

# machines to translate between gloss and algonquian
gloss2alg_machine = (gloss2analysis_machine @ analysis2alg_machine).optimize()
print('generated gloss2alg_machine')
gloss2alg_machine.write(lib_path+'gloss2alg.fst')
print('saved gloss2alg.fst')

alg2gloss_machine = invert(gloss2alg_machine).optimize()
print('generated alg2gloss_machine')
alg2gloss_machine.write(lib_path+'alg2gloss.fst')
print('saved alg2gloss.fst')

def gloss2analysis(text):
    return apply_machine(gloss2analysis_machine,text)
def gloss2alg(text):
    return apply_machine(gloss2alg_machine,text)

def list_string_set(acceptor):
    my_list = []
    paths = acceptor.paths()
    for s in paths.ostrings():
        my_list.append(s)
    my_list.sort(key=len)
    return my_list

def apply_machine(machine, text):
    try:
        return (A(text) @ machine).string(token_type='utf8')
    except:
        return list_string_set(A(text) @ machine)

###########################
##### Build gloss2eng #####
###########################
print('\nBuilding gloss2eng')
print('--------------------')

#######
# stems
#######

eng_stems_from_glossary = [w.eng_stem for w in glossary_words]
def build_eng_stems(words):
    temp = zero
    for w in words: temp = A(w)|temp
    return temp.optimize()
eng_stem = build_eng_stems(eng_stems_from_glossary)

valid_gloss = eng_stem+bound+any_config

def build_eng_sentence(sub,prim_o,sec_o,eng=None,pol=polarity,before_stem=True):
    if eng:
        if before_stem:
            return (T('',eng)+add_space+other_eng+eng_stem+other_eng+bound+sub+bound+prim_o+bound+sec_o+slash+pol).optimize()
        return (other_eng+eng_stem+other_eng+add_space+T('',eng)+bound+sub+bound+prim_o+bound+sec_o+slash+pol).optimize()
    return (other_eng+space+eng_stem+other_eng+bound+sub+bound+prim_o+bound+sec_o+slash+pol).optimize()

#########
# subject
#########
subject_1S = build_eng_sentence(oneS,FE,FE,'i')
subject_1P = build_eng_sentence(oneP,FE,FE,'we')
subject_12P = build_eng_sentence(weinc,FE,FE,'we-inc')
subject_1 = (subject_1S|subject_1P|subject_12P).optimize()

subject_2S = build_eng_sentence(twoS,FE,FE,'you')
subject_2P = build_eng_sentence(twoP,FE,FE,'yall')
subject_2 = (subject_2S|subject_2P).optimize()

subject_DAS = build_eng_sentence(DAS,FE,FE,'that na')
subject_DAP = build_eng_sentence(DAP,FE,FE,'those nas')
subject_DNS = build_eng_sentence(DNS,FE,FE,'that ni')
subject_DNP = build_eng_sentence(DNP,FE,FE,'those nis')
subject_IAS = build_eng_sentence(IAS,FE,FE,'a na')
subject_IAP = build_eng_sentence(IAP,FE,FE,'some nas')
subject_INS = build_eng_sentence(INS,FE,FE,'a ni')
subject_INP = build_eng_sentence(INP,FE,FE,'some nis')
subject_3 = (subject_DAS|subject_DAP|subject_DNS|subject_DNP|
             subject_IAS|subject_IAP|subject_INS|subject_INP)

subject = (subject_1|subject_2|subject_3).optimize()

################
# primary object
################

prim_obj_none = build_eng_sentence(F,E,FE)

prim_obj_1S = build_eng_sentence(F,oneS,FE,'me',before_stem=False)
prim_obj_1P = build_eng_sentence(F,oneP,FE,'us',before_stem=False)
prim_obj_12P = build_eng_sentence(F,weinc,FE,'us-inc',before_stem=False)
prim_obj_1 = (prim_obj_1S|prim_obj_1P|prim_obj_12P).optimize()

prim_obj_2S = build_eng_sentence(F,twoS,FE,'you',before_stem=False)
prim_obj_2P = build_eng_sentence(F,twoP,FE,'yall',before_stem=False)
prim_obj_2 = (prim_obj_2S|prim_obj_2P).optimize()

prim_obj_DAS = build_eng_sentence(F,DAS,FE,'that na',before_stem=False)
prim_obj_DAP = build_eng_sentence(F,DAP,FE,'those nas',before_stem=False)
prim_obj_DNS = build_eng_sentence(F,DNS,FE,'that ni',before_stem=False)
prim_obj_DNP = build_eng_sentence(F,DNP,FE,'those nis',before_stem=False)
prim_obj_IAS = build_eng_sentence(F,IAS,FE,'a na',before_stem=False)
prim_obj_IAP = build_eng_sentence(F,IAP,FE,'some nas',before_stem=False)
prim_obj_INS = build_eng_sentence(F,INS,FE,'a ni',before_stem=False)
prim_obj_INP = build_eng_sentence(F,INP,FE,'some nis',before_stem=False)
prim_obj_3 = (prim_obj_DAS|prim_obj_DAP|prim_obj_DNS|prim_obj_DNP|
              prim_obj_IAS|prim_obj_IAP|prim_obj_INS|prim_obj_INP).optimize()

primary_object = (prim_obj_none|prim_obj_1|prim_obj_2|prim_obj_3).optimize()

##################
# secondary object
##################

sec_obj_none = build_eng_sentence(F,FE,E)

sec_obj_1S = build_eng_sentence(F,FE,oneS,'me',before_stem=False)
sec_obj_1P = build_eng_sentence(F,FE,oneP,'us',before_stem=False)
sec_obj_12P = build_eng_sentence(F,FE,oneP,'us-inc',before_stem=False)
sec_obj_1 = (sec_obj_1S|sec_obj_1P).optimize()

sec_obj_2S = build_eng_sentence(F,FE,twoS,'you',before_stem=False)
sec_obj_2P = build_eng_sentence(F,FE,twoP,'yall',before_stem=False)
sec_obj_2 = (sec_obj_2S|sec_obj_2P).optimize()

sec_obj_DAS = build_eng_sentence(F,FE,DAS,'that na',before_stem=False)
sec_obj_DAP = build_eng_sentence(F,FE,DAP,'those nas',before_stem=False)
sec_obj_DNS = build_eng_sentence(F,FE,DNS,'that ni',before_stem=False)
sec_obj_DNP = build_eng_sentence(F,FE,DNP,'those nis',before_stem=False)
sec_obj_IAS = build_eng_sentence(F,FE,IAS,'a na',before_stem=False)
sec_obj_IAP = build_eng_sentence(F,FE,IAP,'some nas',before_stem=False)
sec_obj_INS = build_eng_sentence(F,FE,INS,'a ni',before_stem=False)
sec_obj_INP = build_eng_sentence(F,FE,INP,'some nis',before_stem=False)
sec_obj_3 = (sec_obj_DAS|sec_obj_DAP|sec_obj_DNS|sec_obj_DNP|
             sec_obj_IAS|sec_obj_IAP|sec_obj_INS|sec_obj_INP).optimize()

secondary_object = (sec_obj_none|sec_obj_1|sec_obj_2|sec_obj_3)

##################
# english negation
##################
break_multiword_stems = cdrewrite(T('_',' '),sigmaStar,sigmaStar,sigmaStar)
eng_stems_with_spaces = (eng_stem @ break_multiword_stems).project('output')

eng_neg_regular = possible_subjects+space+T('','do not ')+(eng_stems_with_spaces-has(A('be ')))+other_eng+bound+F+bound+FE+bound+FE+slash+negative
eng_neg_be = possible_subjects+space+T('be ','be not ')+other_eng+bound+F+bound+FE+bound+FE+slash+negative
eng_neg_none = possible_subjects+space+eng_stems_with_spaces+other_eng+bound+F+bound+FE+bound+FE+slash+positive

english_negation = (eng_neg_regular|eng_neg_be|eng_neg_none).optimize()

################
# english output
################

remove_gloss = other_eng+cross((bound+F+bound+FE+bound+FE+slash+polarity).project('input'),A(''))

# agreement
one_eng_word = closure(newclass(eng_chars))-A('')
agreement_s_regular = threeSg_subjects+space+one_eng_word+(T('','s')|(T('','s')+space+other_eng))
agreement_none = (possible_subjects-(threeSg_subjects))+space+(one_eng_word|(one_eng_word+space+other_eng))

agreement_fix_does = cdrewrite(T('dos','does'),' ',' ',sigmaStar)
agreement_fix_has = cdrewrite(T('haves','has'),' ',' ',sigmaStar)
agreement_fix_am = cdrewrite(T('i be','i am'),'[BOS]',sigmaStar,sigmaStar)
agreement_fix_is = cdrewrite(T('bes','is'),threeSg_subjects+space,sigmaStar,sigmaStar)
agreement_fix_are = cdrewrite(T('be','are'),are_subjects+space,sigmaStar,sigmaStar)
agreement_fix_be = (agreement_fix_am@agreement_fix_is@agreement_fix_are).optimize()
agreement_fix = (agreement_fix_does@agreement_fix_has@agreement_fix_be).optimize()

agreement = ((agreement_s_regular|agreement_none)@agreement_fix).optimize()

# fixing word order
fix_word_order_none = other_eng@complement(has(A('  ')))

fix_word_order_1S = other_eng+T('  ',' me ')+other_eng+del_space+T('me','')
fix_word_order_1P = other_eng+T('  ',' us ')+other_eng+del_space+T('us','')
fix_word_order_weinc = other_eng+T('  ',' us-inc ')+other_eng+del_space+T('us-inc','')
fix_word_order_1 = (fix_word_order_1S|fix_word_order_1P|fix_word_order_weinc).optimize()

fix_word_order_2S = other_eng+T('  ',' you ')+other_eng+del_space+T('you','')
fix_word_order_2P = other_eng+T('  ',' yall ')+other_eng+del_space+T('yall','')
fix_word_order_2 = (fix_word_order_2S|fix_word_order_2P).optimize()

fix_word_order_DAS = other_eng+T('  ',' that na ')+other_eng+del_space+T('that na','')
fix_word_order_DAP = other_eng+T('  ',' those nas ')+other_eng+del_space+T('those nas','')
fix_word_order_DNS = other_eng+T('  ',' that ni ')+other_eng+del_space+T('that ni','')
fix_word_order_DNP = other_eng+T('  ',' those nis ')+other_eng+del_space+T('those nis','')
fix_word_order_IAS = other_eng+T('  ',' a na ')+other_eng+del_space+T('a na','')
fix_word_order_IAP = other_eng+T('  ',' some nas ')+other_eng+del_space+T('some nas','')
fix_word_order_INS = other_eng+T('  ',' a ni ')+other_eng+del_space+T('a ni','')
fix_word_order_INP = other_eng+T('  ',' some nis ')+other_eng+del_space+T('some nis','')
fix_word_order_3 = (fix_word_order_DAS|fix_word_order_DAP|fix_word_order_DNS|fix_word_order_DNP|
                    fix_word_order_IAS|fix_word_order_IAP|fix_word_order_INS|fix_word_order_INP).optimize()

fix_word_order = (fix_word_order_none|fix_word_order_1|fix_word_order_2|fix_word_order_3).optimize()

# capitalization
cap_map = (T('i','I')|T('w','W')|T('y','Y')|T('t','T')|T('s','S')|T('a','A')).optimize()
capitalize_initial = cdrewrite(cap_map,'[BOS]',A(''),sigmaStar_w_capitals)
capitalize_nina = cdrewrite((T('ni','NI')|T('na','NA')),' ',('[EOS]'|space|'s'),sigmaStar_w_capitals)
capitalize = capitalize_initial@capitalize_nina

# put it all together
english_formatting = (remove_gloss@agreement@fix_word_order@capitalize).optimize()

#############################
## Save gloss/eng machines ##
#############################
print('\nGenerating and saving gloss/eng machines')
print('----------------------------------------')

# machines to translate between gloss and english
gloss2eng_machine = (valid_gloss @ subject @ primary_object @ secondary_object @ break_multiword_stems @ english_negation @ english_formatting).optimize()
print('generated gloss2eng_machine')
gloss2eng_machine.write(lib_path+'gloss2eng.fst')
print('saved gloss2eng.fst')

eng2gloss_machine = invert(gloss2eng_machine).optimize()
print('generated eng2gloss_machine')
eng2gloss_machine.write(lib_path+'eng2gloss.fst')
print('saved eng2gloss.fst')

def gloss2eng(gloss):
    return apply_machine(gloss2eng_machine,gloss)

def eng2gloss(eng):
    return apply_machine(eng2gloss_machine,eng)

#############################
## Save alg/eng machines ##
#############################
print('\nGenerating and saving alg/eng machines')
print('--------------------------------------')

# machines to translate between algonquian and english
# English to Algonquian
eng2alg_machine = (eng2gloss_machine @ gloss2alg_machine).optimize()
print('generated eng2alg_machine')
eng2alg_machine.write(lib_path+'eng2alg.fst')
print('saved eng2alg.fst')

# Algonquian to English
alg2eng_machine = (invert(eng2alg_machine)).optimize()
print('generated alg2eng_machine')
alg2eng_machine.write(lib_path+'alg2eng.fst')
print('saved alg2eng.fst')

def alg2eng(alg):
    return apply_machine(alg2eng_machine,alg)

def eng2alg(eng):
    return apply_machine(eng2alg_machine,eng)
    
#######################
## Build entries.csv ##
#######################
all_engs = list_string_set(alg2eng_machine)
all_glosses = []
all_algs = []

for eng in all_engs:
    try:
        gloss = eng2gloss(eng)
        all_glosses.append(gloss)
        alg = eng2alg(eng)
        all_algs.append(alg)
    except:
        print('problem with',eng)
        
all_analyses = [gloss2analysis(gloss) for gloss in all_glosses]

entries = pd.DataFrame([all_glosses,all_algs,all_analyses,all_engs]).transpose()
entries.columns = ['Gloss','Algonquian','Analysis','English']

entries_path = 'entries.csv'
time = datetime.now()
with open(entries_path,'w') as f:
    f.write('Updated {}\n'.format(time))
entries.to_csv(entries_path, index=False,mode='a')
print('Wrote resulting entries to',entries_path,'at {}.'.format(time))
