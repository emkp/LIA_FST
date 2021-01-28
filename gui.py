'''
A script to launch the dictionary gui
'''

from pynini import *
import csv
from trans_funcs import eng2alg, alg2eng

import tkinter as tk
from tkinter import ttk

###################
#### Build GUI ####
###################

LARGE_FONT = ('Cambria', 18)
MEDIUM_FONT = ('Cambria', 16)

class Translator(tk.Tk):

    def __init__(self, *args, **kwargs):
    
        #initialize tkinter
        tk.Tk.__init__(self, *args, **kwargs)
        
        #add a container
        container = tk.Frame(self)
        
        #add the container to the GUI using pack()
        container.pack(side='top', fill='both', expand=True)
        
        #format the container
        container.grid_rowconfigure(0, weight=1)
        container.grid_columnconfigure(0, weight=1)
        
        # create three frames; a start page and two activity pages
        self.frames = {}
        for F in (StartPage, PageOne, PageTwo): 
            frame = F(container, self)
            self.frames[F] = frame
            frame.grid(row=0, column=0, sticky='nsew')
            
        # start by showing Start Page
        self.show_frame(StartPage)
        
    # define a function to show the desired frame
    def show_frame(self, cont):
        frame = self.frames[cont]
        frame.tkraise()
        
class StartPage(tk.Frame):

    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent)
        
        # heading
        heading = tk.Label(self, text="Welcome to the Algonquian Verb Dictionary",font=LARGE_FONT)
        heading.place(relx=0.5,rely=0.05,anchor='n')
        
        # buttons
        button1 = ttk.Button(self, text='English to Algonquian',
                                   command = lambda: controller.show_frame(PageOne))
        button1.place(relx=0.5,rely=0.35,anchor='n')
        
        button2 = ttk.Button(self, text='Algonquian to English',
                                   command = lambda: controller.show_frame(PageTwo))
        button2.place(relx=0.5,rely=0.5,anchor='n')

class PageOne(tk.Frame):
    '''English to Algonquian'''
    def translate_eng2alg(self,eng):
        alg = eng2alg(eng.strip())
        if type(eng) == type([]):
            message = '\n'.join([a for a in alg])
        else:
            message = alg
        print(message)
        
        special_characters = {'á':'U+00E1','ô':'U+00F4'}
        
        output_label = tk.Label(self, text='Translation:',font=LARGE_FONT)
        output_label.grid(row=10, column=0, sticky='W', padx=10)
        
        output_text = tk.Label(self, text=message,
                               font=LARGE_FONT, wraplength=600)
        output_text.grid(row=12, column=0, sticky='W', padx=10)
        
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent)
        
        # heading
        heading = tk.Label(self, text="Translate English to Algonquian", font=LARGE_FONT)
        heading.grid(row=0, column=0, rowspan=2, padx=10, sticky="W")
        
        # 'back' button
        button1 = ttk.Button(self, text="Back",
                            command=lambda: controller.show_frame(StartPage))
        button1.grid(row=0, column=3, padx=10)
        
        # ask for english to translate
        entry_label = tk.Label(self, text='English sentence to translate:',font=LARGE_FONT)
        entry_label.grid(row=2, column=0, rowspan=2, padx=10, sticky="W")
        
        # text box for entry
        eng_entry = tk.Text(self)
        eng_entry.grid(row=4, column=0, rowspan=1, columnspan=2, padx=5)
        
        # add a submit button which calls the translate_eng2alg function
        submit_button = ttk.Button(self, text='Translate!',
                    command = lambda: self.translate_eng2alg(eng_entry.get('1.0','end')))
        submit_button.grid(row=4,column=3,padx=10)
        
class PageTwo(tk.Frame):
    '''Algonquian to English'''
    def translate_alg2eng(self,alg):
        eng = alg2eng(alg.strip())
        if type(eng) == type([]):
            message = '\n'.join([e for e in eng])
        else:
            message = eng
        
        output_label = tk.Label(self, text='Translation:',font=LARGE_FONT)
        output_label.grid(row=10, column=0, sticky='W', padx=10)
        
        output_text = tk.Label(self, text=message,
                               font=LARGE_FONT, wraplength=600)
        output_text.grid(row=12, column=0, sticky='W', padx=10)
    
    def __init__(self, parent, controller):
        tk.Frame.__init__(self, parent)
        
        heading = tk.Label(self,text="Translate Algonquian to English", font=LARGE_FONT)
        heading.grid(row=0, column=0, rowspan=2, padx=10, sticky="W")
        
        # 'back' button
        button1 = ttk.Button(self, text="Back",
                            command=lambda: controller.show_frame(StartPage))
        button1.grid(row=0, column=3, padx=10)
        
        # ask for english to translate
        entry_label = tk.Label(self, text='Algonquian verb to translate:',font=LARGE_FONT)
        entry_label.grid(row=2, column=0, rowspan=2, padx=10, sticky="W")
        
        # text box for entry
        eng_entry = tk.Text(self)
        eng_entry.grid(row=4, column=0, rowspan=1, columnspan=2, padx=5)
        
        # add a submit button which calls the translate_alg2eng function
        submit_button = ttk.Button(self, text='Translate!',
                   command = lambda: self.translate_alg2eng(eng_entry.get('1.0','end').lower()))
        submit_button.grid(row=4,column=3,padx=10)
        
        
# call the GUI!
translator = Translator()
translator.mainloop()
