'''
A script to build the following FST machines:
	gloss2analysis	analysis2gloss
	analysis2alg	alg2analysis
	gloss2alg	alg2gloss
	gloss2eng	eng2gloss
	eng2analysis	analysis2eng
	eng2alg	alg2eng	
'''
#from prettytable import PrettyTable
from pynini import *
import csv

############
# known bugs
############
# tkinter won't print words with tthe special characters á or ô
# "You eat that NI." comes out as "kumicn" but it should come out as "kumicin";
	# will have to check whether this i-insertion is a productive process.

#########
# setup #
#########

def A(s: str) -> Fst:
    return acceptor(s, token_type="utf8")

def T(upper: str, lower: str) -> Fst:
    return cross(A(upper), A(lower))

a = A("a")
zero = a-a
zero.optimize()

# some natural classes

def newclass(letters:str):
    temp = zero
    for x in letters: temp = A(x) | temp
    return temp.optimize()

vowel = newclass('auáiôo')
consonant = newclass('ktpcwynmsʃhrq')

bound_del = T('.','')
del_bound = T('.','') # need to delete one of these
bound = A('.')
plus = A('+')
space = A(' ')
del_space = T(' ','')
add_plus = T('','+')

gloss_syms = newclass('SPDIAN12E.+ ')
eng_syms = newclass('legdvNIYTW()_- ')

# all segments
sigmaStar = (closure(vowel|consonant|gloss_syms|eng_syms)).optimize()

def complement(machine):
    return difference(sigmaStar, machine)
    
def has(machine):
    return sigmaStar+machine+sigmaStar

######################################
## shortcut transducers: Algonquian ##
######################################

S = A('S') # S singular
P = A('P') # P plural
D = A('D') # D definite
I = A('I') # I indefinite
An = A('A') # A animate 3rd person
N = A('N') # N inanimate 3rd person
one = A('1') # 1 first person
two = A('2') # 2 second person
E = A('E') # E empty

weinc = A('12P') # we inclusive

oneS = one+S 			# singular 1st person
oneP = one+P 			# plural 1st person
oneSP = (oneS|oneP).optimize() # 1st person singular or plural
twoS = two+S			# singular 2nd person
twoP = two+P 			# plural 2nd person
twoSP = (twoS|twoP).optimize() # 2nd person singular or plural

DA = D+An # definite animate 3rd person
DN = D+N  # definite inanimate 3rd person
IA = I+An # indefinite animate 3rd person
IN = I+N # indefinite inanimate 3rd person

DAS = DA+S			# definite animate singular
DAP = DA+P			# definite animate plural
DASP = (DAS|DAP).optimize()	# definite animate singular or plural
DNS = DN+S			# definite inanimate singular
DNP = DN+P			# definite inanimate plural
DNSP = (DNS|DNP).optimize()	# definite inanimate singular or plural
IAS = IA+S			# indefinite animate singular
IAP = IA+P			# indefinite animate plural
IASP = (IAS|IAP).optimize()	# indefinite animate singular or plural
INS = IN+S			# indefinite inanimate singular
INP = IN+P			# indefinite inanimate plural
INSP = (INS|INP).optimize()	# indefinite inanimate singular or plural
NSP = (DNSP|INSP).optimize()	# definite or indefinite inanimate singular or plural


# F any argument but empty (F for Filled)
F =  (oneSP|twoSP|DASP|DNSP|IASP|INSP|weinc).optimize()

# FE any argument including empty
FE = (F|E).optimize()
    
# three third person
threeSP = (DASP|DNSP|IASP|INSP).optimize()
threeP = (DAP|DNP|IAP|INP).optimize()

# ym you and me only: 1S, 1P, 2S, 2P, 12P
ym = (oneSP|twoSP|weinc).optimize()

# NI inanimate 3rd person: DNS, DNP, INS, INP
NI = (DNSP|INSP).optimize()

# NA animate 3rd person: DAS, DAP, IAS, IAP
NA = (DASP|IASP).optimize()

# V visible: 1, 2, DA, DN
V = (oneSP|twoSP|DASP|DNSP|weinc).optimize()
VA = (oneSP|twoSP|weinc|DASP|IASP).optimize()

# V3 visible 3rd person: DA, DN
V3 = (DASP|DNSP).optimize()

# IV3 invisible 3rd person: IAS, IAP, INS, INP
IV3 = (IASP|INSP).optimize()
    
# X invisible: IA, IN, E
X = (IASP|INSP|E).optimize()

#####################################
### shortcut transducers: English ###
#####################################

# shortcut transducers for English
that_ni = A('that ni')
those_nis = A('those nis')
D_ni = (that_ni|those_nis)

that_na = A('that na')
those_nas = A('those nas')
D_na = (that_na|those_nas)

a_ni = A('a ni')
some_nis = A('some nis')
I_ni = (a_ni|some_nis)

a_na = A('a na')
some_nas = A('some nas')
I_na = (a_na|some_nas)

threeSP_eng = (D_ni|D_na|I_ni|I_na).optimize()

oneSP_eng_subj = (A('i')|('we'))
oneSP_eng_obj = (A('me')|('us'))
twoSP_eng = (A('you')|A('yall'))
weinc_eng_subj = A('we-inc')
weinc_eng_obj = A('us-inc')
prim_obj_engs_subj = (threeSP_eng|oneSP_eng_subj|twoSP_eng|weinc_eng_subj).optimize()
prim_obj_engs_obj = (threeSP_eng|oneSP_eng_obj|twoSP_eng|weinc_eng_obj).optimize()

######################################
## Getting attested forms from data ##
######################################
# should this be in its own file?

path_to_project = "/home/ekp/Documents/SBU_Fall2020/Thesis/Thesis_code/LIA_FST/"
eng_vocab_file = open(path_to_project+"eng_vocab.txt")
eng_vocab = eng_vocab_file.readlines()

forms_from_data = {}
for e in eng_vocab:
    e = e.strip()
    e_path = path_to_project+'forms_from_data/'+e+'.csv'
    with open(e_path, newline='') as csvfile:
        csvreader = csv.reader(csvfile,delimiter=',')
        from_data = []
        for row in csvreader:
            from_data.append(tuple(row))
        from_data = from_data[1:]
        from_data_dict = {}
        for x in from_data:
            alg_form = ''
            for c in x[0]:
                if c != 'ȏ':
                    alg_form += c
                else:
                    alg_form += 'ô'
            from_data_dict[x[1]] = alg_form
    forms_from_data[e] = from_data_dict
#print(forms_from_data)

##########################################
## Getting the generated configurations ##
##########################################
# should this be in its own file?

# config_types_dict is a dictionary with names of transitivity types as keys
# and lists of possible configurations for that kind of transitivity as values
config_types = ['ditrans', 'monotrans', 'intrans']
config_types_dict = {}
for c in config_types:
    c_path = path_to_project+'configs/'+c+'_configs.txt'
    configs_file = open(c_path)
    configs = configs_file.readlines()
    config_types_dict[c] = configs

###########################
##### Build gloss2alg #####
###########################
print('\nBuilding gloss2alg machine')
print('----------------------------')

#######
# stems
#######

class Word:
    
    def __init__(self, eng_stem, transitivity, alg_TI_stem=None, 
                 alg_TA_stem=None, alg_AI_stem=None, alg_II_stem=None, regular=True):
        self.eng_stem = eng_stem
        self.transitivity = transitivity
        self.alg_TA_stem = alg_TA_stem
        self.alg_TI_stem = alg_TI_stem
        self.alg_AI_stem = alg_AI_stem
        self.alg_II_stem = alg_II_stem
        self.regular = regular
        self.forms = [self.alg_TA_stem, self.alg_TI_stem, self.alg_AI_stem,
                self.alg_II_stem]
        self.num_forms = len([f for f in self.forms if f!=None])
    
    def summary(self):
        print("eng stem:", self.eng_stem)
        print("transitivity:", self.transitivity)
        print("number of forms:", self.num_forms)
        if self.alg_TA_stem:
            print("TA", self.alg_TA_stem)
        if self.alg_TI_stem:
            print("TI", self.alg_TI_stem)
        if self.alg_AI_stem:
            print("AI", self.alg_AI_stem)
        if self.alg_II_stem:
            print("II", self.alg_II_stem)
        print('regular:', self.regular)
        print()
        
with open('glossary.txt', 'r', errors='replace') as f:
    glossary = f.readlines()[1:]
    
words = []

def rep_spec(word):
    ''' replace special characters ô
    '''
    new_word = ''
    for c in word:
        if c == "ô":
            new_word += 'ô'
        else:
            new_word += c
    return new_word        
        
for line in glossary:
    eng_stem = None
    transitivity = None
    TI = None
    TA = None
    AI = None
    II = None
    regular = True
    
    line = line.strip().split('\t')
    eng_stem = line[0]
    transitivity = line[1]
    if line[-1] == 'irregular':
        regular = False
        line.pop(-1)
    forms = line[2:]
    it = iter(forms)
    forms = list(zip(it, it))
    for tup in forms:
        if tup[0] == 'TA':
            TA = tup[1]
        if tup[0] == 'TI':
            TI = tup[1]
        if tup[0] == 'AI':
            AI = tup[1]
        if tup[0] == 'II':
            II = tup[1]
        if tup[0] == 'TA/TI':
            TA = tup[1]
            
        new_word = Word(eng_stem, transitivity, TI, TA, AI, II, regular)
    words.append(new_word)
    
# stems with irregular alternations
print('Irregular stems')
print('---------------')
irregular_words = [w for w in words if w.regular==False]
print([w.eng_stem for w in irregular_words])
mis_context = twoSP+bound+oneSP+bound+F
mir_mis = ((T('give','mis')+del_bound+add_plus+mis_context)|(T('give','mir')+del_bound+add_plus+complement(mis_context))).optimize()
irreg_stems = mir_mis

# regular stems
regular_words = [w for w in words if w.regular==True]

simple_ditrans_words = [w for w in regular_words if w.num_forms==1 and \
                        w.regular==True and w.transitivity=='ditrans']
simple_monotrans_words = [w for w in regular_words if w.num_forms==1 and \
                          w.regular==True and w.transitivity=='monotrans']
simple_intrans_words = [w for w in regular_words if w.num_forms==1 and \
                        w.regular==True and w.transitivity=='intrans']

print('\nSimple stems')
print('------------')
def build_simple_stems(wordlist, transitivity):
    temp = zero
    for w in wordlist:
        print(w.eng_stem)
        for f in w.forms:
            if f:
                new_T = T(w.eng_stem, f)
                temp = temp | new_T

    if transitivity == 'ditrans':
        context = F+bound+F+bound+F
    elif transitivity == 'monotrans':
        context = F+bound+F+bound+E
    elif transitivity == 'intrans':
        context = F+bound+E+bound+E
        
    simple_stems = temp+del_bound+add_plus+context
    
    return simple_stems.optimize()

simple_ditrans = build_simple_stems(simple_ditrans_words, "ditrans")
simple_monotrans = build_simple_stems(simple_monotrans_words, "monotrans")
simple_intrans = build_simple_stems(simple_intrans_words, "intrans")

# stems that alternate based on animacy of the object
print('\nAlternating stems')
print('-----------------')
alternating_words = [w for w in regular_words if w.num_forms>1]

def build_alternating(stem,TI_form,TA_form,AI_form,II_form):
    print('stem:', stem)
    if TI_form and TA_form and not AI_form:
        print('TI and TA')
        print('TI_form:',TI_form)
        print('TA_form:',TA_form)
        TI_context = F+bound+NSP+bound+E
        TA_context = F+bound+VA+bound+E
        TI = (T(stem,TI_form)+del_bound+add_plus+TI_context)
        alternating = (T(stem,TI_form)+del_bound+add_plus+TI_context)|\
                    (T(stem,TA_form)+del_bound+add_plus+TA_context)
        
    if TI_form and TA_form and AI_form:
        print('TI and TA and AI')
        TI_context = F+bound+NSP+bound+E
        AI_context = VA+bound+E+bound+E
        TI_AI = (TI_context|AI_context).optimize()
        alternating = (T(stem,TI_form)+del_bound+add_plus+TI_context)|\
                (T(stem,AI_form)+del_bound+add_plus+AI_context)|\
                (T(stem,TA_form)+del_bound+add_plus+complement(TI_AI))
        
    if II_form and AI_form and not TI_form:
        print('II and AI')
        print('II:', II_form)
        print('AI:', AI_form)
        II_context = NSP+bound+E+bound+E
        AI_context = VA+bound+E+bound+E
        alternating = (T(stem,II_form)+del_bound+add_plus+II_context)|\
                    (T(stem,AI_form)+del_bound+add_plus+AI_context)
    return alternating.optimize()

def build_alternating_stems(wordlist):
    temp = zero
    for w in wordlist:
        print('word:',w.eng_stem)
        new_T = build_alternating(w.eng_stem,w.alg_TI_stem,w.alg_TA_stem,w.alg_AI_stem,w.alg_II_stem)
        temp = temp | new_T
        print()
    return temp.optimize()

alternating_stems = build_alternating_stems(alternating_words)

stems = (irreg_stems | simple_ditrans | simple_monotrans | simple_intrans | alternating_stems).optimize()

################
# central suffix
################
print('generating central suffix machines')

# N if there are 3 visible args: V V V
# OR if it's monotransitive with def NI obj:
    # V DN(SP) ?? or F DN(SP) going with F DN(SP) for now
        # need to know centsuf for (indefNA defNI empty) i.e. 'IAS.2P.E'
centsuf_N1 = V+bound+V+bound+V
centsuf_N2 = F+bound+DNSP+bound+X
centsuf_N = (centsuf_N1|centsuf_N2).optimize()+T('','+ná')

# W if there is one+ invisible arg and one def (V X)|(X V) and not N2
# OR all invisible: X X X
#centsuf_W1 = sigmaStar+V3+sigmaStar+X+sigmaStar
centsuf_W1 = complement(centsuf_N2) @ (sigmaStar+V3+sigmaStar+X+sigmaStar)
centsuf_W2 = complement(centsuf_N2) @ (sigmaStar+X+sigmaStar+V3+sigmaStar)
centsuf_W3 = sigmaStar+X+bound+X+bound+X
centsuf_W = complement(sigmaStar+plus+centsuf_N2) @ (centsuf_W1|centsuf_W2|centsuf_W3).optimize()+T('','+w')
# is this the best way to do this?

# M if there are only 1/2 arguments; i.e. comp(has_3)
has_V3 = has(V3)
centsuf_M = (complement(centsuf_W3) @ complement(has_V3)).optimize()+T('','+m')

centsuf = sigmaStar+plus+(centsuf_N|centsuf_W|centsuf_M).optimize()
centsuf_NM = sigmaStar+plus+(centsuf_N|centsuf_M)

########
# prefix
########
print('generating prefix machines')

# get ku if a 2nd-person arg is involved
has_2 = has(two)
prefix_ku = T('','ku+') + has_2

# get nu if there's no 2nd-person but there is a 1st-person
has_1 = has(one)
prefix_nu = T('','nu+') + (complement(has_2) @ has_1)

# get no prefix if it's 3rd-person intransitive
prefix_none = sigmaStar+threeSP+bound+E+bound+E+sigmaStar

# get wu if there's no 2nd- or 1st- person, and it's not 3rd-person intransitive
prefix_wu = complement(prefix_none) @ (T('','wu+') + (complement(has_2) @ complement(has_1))).optimize()

# exceptions: some stems that don't get prefixes
# still don't know if this is a productive exception?
#prefix_exceptions = project(A('apu')+sigmaStar,'input')
#regular_prefix = (sigmaStar - prefix_exceptions).optimize()

#prefix = (prefix_exceptions | (regular_prefix @ (prefix_ku|prefix_nu|prefix_wu))).optimize()
prefix = (prefix_ku|prefix_nu|prefix_wu|prefix_none).optimize()

######################
# personal pluralizers
######################
print('generating personal pluralizers machines')

# get non if it has 1P or 12P
# since 1Pl will never show up in B, can just search whole string for it:
has_1P = has(oneP)
has_weinc = has(weinc)
#pp_non = has_1P + add_plus+T('','nôn')
pp_non = (has_1P|has_weinc) + add_plus+T('','nôn')

# get wow if it doesn't have 1P but does have 2P (but not 12P) or [3AP and one other arg], not counting arg B.
has_2P = has(twoP)
threeAP = (DAP|IAP).optimize()
pp_wow1 = has_2P
pp_wow2 = threeAP+bound+F+bound+FE
pp_wow3 = F+bound+threeAP+bound+FE
pp_wow = complement(has_1P|has_weinc) @ (sigmaStar+(pp_wow1|pp_wow2|pp_wow3)+sigmaStar).optimize()+add_plus+T('','wôw')

# get no personal pluralizer if has no P (not counting last arg) or if intransitive with 3rd-person Pl arg
has_P = has(P)
threeNP = (DNP|INP).optimize()
pp_all_sg = complement(has_P)+bound+(threeSP|E)+plus+sigmaStar
pp_intrans_3 = sigmaStar+threeP+bound+E+bound+E+plus+sigmaStar
pp_monotrans_NI = sigmaStar+S+bound+threeNP+bound+E+plus+sigmaStar
pp_none = (pp_all_sg|pp_intrans_3|pp_monotrans_NI).optimize()

personal_plural = (pp_wow | pp_non | pp_none).optimize()

#############
# theme signs
#############
print('generating theme signs machines')

u = T('','u')
uru = T('','uru')
uko = T('','uko')
on = T('','ô')

# u if 2.1.FE
ts_u = (twoSP|weinc)+bound+oneSP+bound+FE+add_plus+u

# uru if 1.2.FE
ts_uru = oneSP+bound+(twoSP|weinc)+bound+FE+add_plus+uru

# ô if F.3.FE
ts_on = F+bound+NA+bound+FE+add_plus+on

# uko if 3.F.FE and not F.3.FE
ts_uko = threeSP+bound+(F-threeSP)+bound+FE+add_plus+uko

# none if F.E.E
ts_none_1 = F+bound+E+bound+E
ts_none_2 = F+bound+NI+bound+E
ts_none = (ts_none_1|ts_none_2).optimize()

theme_sign = (sigmaStar+(ts_u|ts_uru|ts_uko|ts_on|ts_none)+sigmaStar).optimize()

##############
# remove gloss from final form; this includings reducing ++ to +
##############

to_remove = cross((F+bound+FE+bound+FE).project('input'),'')
reduce_plusses = cdrewrite(T('++','+'),sigmaStar,sigmaStar,sigmaStar)
remove_gloss = cdrewrite(cross(to_remove,''), plus, plus, sigmaStar) @ reduce_plusses

# peripheral suffixes: the object pluralizer and the obviative marker
# which one takes precedence?

###################
# object pluralizer
###################
print('generating object pluralizer machines')

# ak if centsuf is not M (requirements of DAP below takes care of this) AND
op_ak_ditran = DAP+plus # ditransitive and objB is DAP
op_ak_mntran = DAP+bound+E+plus # monotransitive and objA is DAP
op_ak_intran = DAP+bound+E+bound+E+plus # intransitive and sub is DAP
op_ak_conditions = (sigmaStar+(op_ak_ditran|op_ak_mntran|op_ak_intran)+sigmaStar).optimize()
op_ak = op_ak_conditions + add_plus + T('','ak')

# ash if centsuf is not M (requirements of DAP below takes care of this) AND
op_ash_ditran = DNP+plus # ditransitive and objB is DNP
op_ash_mntran = DNP+bound+E+plus # monotransitive and objA is DNP
op_ash_intran = DNP+bound+E+bound+E+plus # intransitive and sub is DNP
op_ash_conditions = (sigmaStar+(op_ash_ditran|op_ash_mntran|op_ash_intran)+sigmaStar).optimize()
op_ash = op_ash_conditions + add_plus + T('','ash')

# none if not these conditions
op_none = (complement(op_ak_conditions)@complement(op_ash_conditions))

object_plural = (op_ak|op_ash|op_none).optimize()

##################
# obviative marker
##################
print('generating obviative marker machines')

# ah if centsuf is not M AND there are two or more DASP
# there must be a better way to do this
# implicity applies the condition that centsuf can't be M because DASP is required
has_three_DASPs = has(DASP+sigmaStar+DASP+sigmaStar+DASP)
has_two_DASPs = has(DASP+sigmaStar+DASP)
obviative_ah = (has_three_DASPs|(complement(has_three_DASPs)@has_two_DASPs))+add_plus+T('','ah')

# none if not these conditions
obviative_none = complement(has(DASP+sigmaStar+DASP))

obviative = (obviative_ah|obviative_none).optimize()

# if obviative takes precedence:
peripherals = (obviative_ah | (obviative_none @ object_plural)).optimize()

# if object_plural takes precedence:
# peripherals = (object_plural | (complement(object_plural) @ obviative)).optimize()

##############################
# phonology and spelling rules
##############################
print('generating phonology and spelling rules machines')

# first remove plusses
remove_plusses = cdrewrite(T('+',''),sigmaStar,sigmaStar,sigmaStar)


# insertion processes: intrusive t and u-bridge
# insert a t between u and a
intrusive_t = cdrewrite(T('','t'), A('u'), A('a'), sigmaStar)

# insert a u in: mn, mw, wn, ww
u_bridge = cdrewrite(u,A('m')|A('w')|A('s'),A('n')|A('w')|A('m'),sigmaStar) # it might be more productive than this?...
# not sure whether ww should be handled as wuw or w; see ku+mir+uko+w+wôw=kumirukowuw vs ku+mir+ô+w+wôw=kumirôw

insertion = (intrusive_t @ u_bridge)

no_double_a = cdrewrite(T('áa','a'),sigmaStar,sigmaStar,sigmaStar)
no_doubles = no_double_a #@ no_double_w

# truncation processes: delete final a, onn, and onw
delete_final_a = cdrewrite(T("á",''),sigmaStar,'[EOS]',sigmaStar)
delete_final_um = cdrewrite(T("um",''),sigmaStar,'[EOS]',sigmaStar)

truncate_final_nonn = cdrewrite(T('ôn',''),'n','[EOS]',sigmaStar)
truncate_final_wonw = cdrewrite(T('ôw',''),'w','[EOS]',sigmaStar) # need to figure out when wonw and nonw truncation happens
truncate_final_w = cdrewrite(T('w',''),'t','[EOS]',sigmaStar)

truncation = (truncate_final_nonn @ truncate_final_wonw @ delete_final_a @ delete_final_um).optimize()

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

lib_path = path_to_project+'/lib/'

# machines to translate between gloss and analysis
gloss2analysis_machine = (stems @ centsuf @ prefix @ personal_plural @ theme_sign @ peripherals @ remove_gloss).optimize()
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


###########################
##### Build gloss2eng #####
###########################
print('\nBuilding gloss2eng')
print('--------------------')

#######
# stems
#######
print('Generating English stems machine')

eng_stems_from_glossary = [l.split('\t')[0] for l in glossary]
def build_eng_stems(words:list):
    temp = zero
    for w in words: temp = A(w)|temp
    return temp.optimize()
eng_stem = build_eng_stems(eng_stems_from_glossary)

#########
# subject
#########
print('Generating English subject machine')
subject_filler_it = eng_stem
subject_filler_mt = eng_stem+space+prim_obj_engs_obj
subject_filler_dt = eng_stem+space+prim_obj_engs_obj+space+threeSP_eng
subject_filler = (subject_filler_it|subject_filler_mt|subject_filler_dt).optimize()

# you -> 2S
sub_2S = T('you ','')+subject_filler+T('','.2S.')
sub_2P = T('yall ','')+subject_filler+T('','.2P.')
sub_2 = (sub_2S|sub_2P).optimize()

# I -> 1S
sub_1S = T('i ','')+subject_filler+T('','.1S.')
sub_1P = T('we ','')+subject_filler+T('','.1P.')
sub_1 = (sub_1S|sub_1P).optimize()

sub_12P = T('we-inc ','')+subject_filler+T('','.12P.')

# 3rd person
sub_DNS = T('that ni ','')+subject_filler+T('','.DNS.')
sub_DNP = T('those nis ','')+subject_filler+T('','.DNP.')
sub_INS = T('some ni ','')+subject_filler+T('','.INS.')
sub_INP = T('some nis ','')+subject_filler+T('','.INP.')
sub_DAS = T('that na ','')+subject_filler+T('','.DAS.')
sub_DAP = T('those nas ','')+subject_filler+T('','.DAP.')
sub_IAS = T('a na ','')+subject_filler+T('','.IAS.')
sub_IAP = T('some nas ','')+subject_filler+T('','.IAP.')

sub_3 = (sub_DNS|sub_DNP|sub_INS|sub_INP|sub_DAS|sub_DAP|sub_IAS|sub_IAP).optimize()

subject = (sub_2|sub_1|sub_12P|sub_3).optimize()

################
# primary object
################
print('Generating English primary object machine')

prim_obj_filler_mit = bound+F+bound
prim_obj_filler_dt = del_space+(threeSP_eng+bound+F+bound).optimize()
prim_obj_filler = (prim_obj_filler_dt|prim_obj_filler_mit).optimize()

prim_obj_it = eng_stem+prim_obj_filler_mit+T('','E')

prim_obj_2S_dt = eng_stem+space+T('you','')+prim_obj_filler_dt+T('','2S')
prim_obj_2S_mt = eng_stem+del_space+T('you','')+prim_obj_filler_mit+T('','2S')
prim_obj_2S = (prim_obj_2S_mt|prim_obj_2S_dt).optimize()

prim_obj_2P_dt = eng_stem+space+T('yall','')+prim_obj_filler_dt+T('','2P')
prim_obj_2P_mt = eng_stem+space+T('yall','')+prim_obj_filler+T('','2P')
prim_obj_2P = (prim_obj_2P_mt|prim_obj_2P_dt).optimize()

prim_obj_2 = (prim_obj_2S|prim_obj_2P).optimize()

prim_obj_1S_dt = eng_stem+space+T('me','')+prim_obj_filler+T('','1S')
prim_obj_1S_mt = eng_stem+del_space+T('me','')+prim_obj_filler_mit+T('','1S')
prim_obj_1S = (prim_obj_1S_mt|prim_obj_1S_dt).optimize()

prim_obj_1P_dt = eng_stem+space+T('us','')+prim_obj_filler+T('','1P')
prim_obj_1P_mt = eng_stem+space+T('us','')+prim_obj_filler+T('','1P')
prim_obj_1P = (prim_obj_1P_mt|prim_obj_1P_dt).optimize()

prim_obj_1 = (prim_obj_1S|prim_obj_1P).optimize()

prim_obj_12P = eng_stem+space+T('us-inc','')+prim_obj_filler+T('','12P')

prim_obj_DNS = eng_stem+space+T('that ni','')+prim_obj_filler+T('','DNS')
prim_obj_DNP = eng_stem+space+T('those nis','')+prim_obj_filler+T('','DNP')
prim_obj_DAS = eng_stem+space+T('that na','')+prim_obj_filler+T('','DAS')
prim_obj_DAP = eng_stem+space+T('those nas','')+prim_obj_filler+T('','DAP')
prim_obj_INS = eng_stem+space+T('a ni','')+prim_obj_filler+T('','INS')
prim_obj_INP = eng_stem+space+T('some nis','')+prim_obj_filler+T('','INP')
prim_obj_IAS = eng_stem+space+T('a na','')+prim_obj_filler+T('','IAS')
prim_obj_IAP = eng_stem+space+T('some nas','')+prim_obj_filler+T('','IAP')
prim_obj_3 = (prim_obj_DNS|prim_obj_DNP|prim_obj_DAS|prim_obj_DAP|prim_obj_INS|prim_obj_INP|prim_obj_IAS|prim_obj_IAP).optimize()

prim_obj = (prim_obj_2|prim_obj_1|prim_obj_12P|prim_obj_3|prim_obj_it).optimize()

##################
# secondary object
##################
print('Generating English secondary object machine')

FEFE = bound+FE+bound+FE
sec_obj_none = eng_stem+(del_space|A(''))+FEFE+T('','.E')
sec_obj_DNS = eng_stem+(T(' that ni',''))+FEFE+T('','.DNS')
sec_obj_DNP = eng_stem+(T(' those nis',''))+FEFE+T('','.DNP')
sec_obj_DAS = eng_stem+(T(' that na',''))+FEFE+T('','.DAS')
sec_obj_DAP = eng_stem+(T(' those nas',''))+FEFE+T('','.DAP')
sec_obj_INS = eng_stem+(T(' a ni',''))+FEFE+T('','.INS')
sec_obj_INP = eng_stem+(T(' some nis',''))+FEFE+T('','.INP')
sec_obj_IAS = eng_stem+(T(' a na',''))+FEFE+T('','.IAS')
sec_obj_IAP = eng_stem+(T(' some nas',''))+FEFE+T('','.IAP')
sec_obj = (sec_obj_none|sec_obj_DNS|sec_obj_DNP|sec_obj_DAS|sec_obj_DAP|sec_obj_INS|sec_obj_INP|sec_obj_IAS|sec_obj_IAP).optimize()

###########################
# English output formatting
###########################
print('Generating English output formatting machine')

# agreement
# multiword stems need to have the agreeing 's' on the first word
split_multiword_stems = cdrewrite(T('_',' '),sigmaStar,sigmaStar-A('inc'),sigmaStar)
context_stems = project(eng_stem @ split_multiword_stems, "output")
del_sigmaStar = cross(sigmaStar.project('input'),'')
no_second_word = ((sigmaStar + T(' ','')+del_sigmaStar)|complement(space)).optimize()
context_stems = (context_stems @ no_second_word).project('output')

agree_context = (A('ni')|A('na'))+space+context_stems
agreement = cdrewrite(T('','s'),agree_context,sigmaStar,sigmaStar)

# capitalization
capital_i = cdrewrite(T('i','I'), '[BOS]', A(' '), sigmaStar)
capital_ni = cdrewrite(T('ni','NI'), A(' '), A(''), sigmaStar)
capital_na = cdrewrite(T('na','NA'), A(' '), A(''), sigmaStar)
capitalized = T('y','Y')|T('t','T')|T('a','A')|T('w','W')|T('s','S')
capital_first = cdrewrite(capitalized,'[BOS]', A(''), sigmaStar)
capital = (capital_i @ capital_ni @ capital_na @ capital_first).optimize()

# put it all together
english_output_format = split_multiword_stems @ agreement @ (capital + T('','.'))

#############################
## Save gloss/eng machines ##
#############################
print('\nGenerating and saving gloss/eng machines')
print('----------------------------------------')

# machines to translate between gloss and english
gloss2eng_machine = (invert(subject @ prim_obj @ sec_obj) @ english_output_format).optimize()
print('generated gloss2eng_machine')
gloss2eng_machine.write(lib_path+'gloss2eng.fst')
print('saved gloss2eng.fst')

eng2gloss_machine = invert(gloss2eng_machine).optimize()
print('generated eng2gloss_machine')
eng2gloss_machine.write(lib_path+'eng2gloss.fst')
print('saved eng2gloss.fst')

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

