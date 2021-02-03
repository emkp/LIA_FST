######################################
## Getting attested forms from data ##
######################################
# should this be in its own file?

path_to_project = "/home/ekp/Documents/School/SBU_Fall2020/Thesis/Thesis_code/LIA_FST/"
'''eng_vocab_file = open(path_to_project+"eng_vocab.txt")
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
    forms_from_data[e] = from_data_dict'''
#print(forms_from_data)

##########################################
## Getting the generated configurations ##
##########################################
# should this be in its own file?

# config_types_dict is a dictionary with names of transitivity types as keys
# and lists of possible configurations for that kind of transitivity as values
'''config_types = ['ditrans', 'monotrans', 'intrans']
config_types_dict = {}
for c in config_types:
    c_path = path_to_project+'configs/'+c+'_configs.txt'
    configs_file = open(c_path)
    configs = configs_file.readlines()
    config_types_dict[c] = configs'''
