

from tkinter import * # python 3.4
import sqlite3
from SQL import *


conn = sqlite3.connect("PROJET.sqlite")


"""recherche de team"""

def rechercheteam():

    """fenetre de recherche de team"""
    
    c = conn.cursor()

    c.execute("""SELECT id FROM TEAM""")

    equipes = c.fetchall()




    t=Toplevel()
    can2 = Canvas(t, width=1024, height=512)
    photo2 = PhotoImage(file="recherche_team.png")
    can2.pack()
    t.resizable(False,False)
    a=Button(t,text='retour',command=t.destroy)
    can2.create_image(0, 0, anchor=NW, image=photo2)
    a.pack()

    teams = Listbox(t,height = 22, width  =  50)

    teams_id = []
    for x in range (0,len(equipes)):
        nom = nom_equipes(equipes[x][0])
        while nom in teams_id:
            nom += "*"

        teams_id.append(nom)
        teams.insert(x+1,nom)
    
    teams_w = can2.create_window(300, 250, window=teams)

    teams.bind('<Double-Button-1>', lambda x: team(t,teams.get(teams.curselection())) )

    DH2014var = IntVar() #variable valant 1 ou 0 celon la valeur du bouton
    Katowice2015var = IntVar()
    Cologne2015var = IntVar()
    CN2015var = IntVar()
    Columbusvar = IntVar()
    Cologne2016var = IntVar()
    ELeague2017var = IntVar()
    var1 = StringVar()
    def click2():
        """action pour un bouton"""
        actualiserteams(teams,equipes,s.get(),"",False,DH2014var.get(),Katowice2015var.get(),Cologne2015var.get(),CN2015var.get(),Columbusvar.get(),Cologne2016var.get(),ELeague2017var.get(),var1.get())



    """"//// Les boutons\\\\\""""
    DH2014 = Checkbutton(t,variable = DH2014var,command = click2)
    DH2014_w = can2.create_window(835, 365, window=DH2014)

    Katowice2015 = Checkbutton(t,variable = Katowice2015var,command = click2)
    Katowice2015_w = can2.create_window(835, 390, window=Katowice2015)

    Cologne2015 = Checkbutton(t,variable = Cologne2015var,command = click2)
    Cologne2015_w = can2.create_window(835, 415, window=Cologne2015)

    CN2015 = Checkbutton(t,variable = CN2015var,command = click2)
    CN2015_w = can2.create_window(865, 440, window=CN2015)

    Columbus = Checkbutton(t,variable = Columbusvar,command = click2)
    Columbus_w = can2.create_window(865, 465, window=Columbus)

    Cologne2016 = Checkbutton(t,variable = Cologne2016var,command = click2)
    Cologne2016_w = can2.create_window(865, 490, window=Cologne2016)

    ELeague2017 = Checkbutton(t,variable = ELeague2017var,command = click2)
    ELeague2017_w = can2.create_window(1005, 365, window=ELeague2017)



    nationalite = ["-None-","be","bosnia","brazil","ca","den","esp","est","fin","fr","hun","kaza","nor","neth","pol","port","czech","en","ru","slov","swed","ukr","us","ger","aust","switz"]
           

    nationalite_l = OptionMenu(t,var1,*nationalite,command = lambda x: click2())
    nationalite_w = can2.create_window(800, 240, window=nationalite_l)
    

    s = StringVar()
    entree = Entry(t, textvariable= s, width=30)
    entree_w = can2.create_window(800, 130, window=entree)


    def click(key):
        """action a effectuer lorsque l'on clique sur un bouton"""
        test = True
        if  key.keycode  == 8:	
            test = False
        actualiserteams(teams,equipes,s.get(),key.char,test,DH2014var.get(),Katowice2015var.get(),Cologne2015var.get(),CN2015var.get(),Columbusvar.get(),Cologne2016var.get(),ELeague2017var.get(),var1.get())



        
    entree.bind("<Key>", click)
    t.mainloop()


def actualiserteams(teams,equipes,text,key,test,DH2014,Katowice2015,Cologne2015,CN2015,Columbus,Cologne2016,ELeague2017,nat):
     """ permet d'actualiser la recherche"""

     teams.delete(0,22)

     team = []

     compet = []

     def appart_team(compet,x):
         """ regarde si l'équipe a participer a la competition"""
         if compet == []:
            compet2 = a_participe(x)

         else:
            compet2 = []
            for x in a_participe(x):
                if x in compet:
                    compet2.append(x)
         return compet2

     #ACTUALISE CELON LA VALEUR DES BOUTONS
     if DH2014 == 1:    
        compet2 = appart_team(compet,1)
        compet = compet2
     if Katowice2015 ==1:
        compet = appart_team(compet,2)
     if Cologne2015 ==1:
        compet = appart_team(compet,3)
     if CN2015 ==1:
        compet = appart_team(compet,4) 
     if Columbus == 1:
        compet = appart_team(compet,5)  
     if Cologne2016 == 1:
        compet = appart_team(compet,6)  
     if ELeague2017 == 1:
        compet = appart_team(compet,7)

     for x in range(len(equipes)):
         if compet == []:
            team.append((nom_equipes(equipes[x][0]),))
         elif equipes[x][0] in compet:
             team.append((nom_equipes(equipes[x][0]),))
     equipes = team


    #FILTRAGE DE LA VALEUR RENTREE
         
     n_teams = []
     if test == True:
        text = text + key

     else:
         nt = ""
         for i in range(len(text) - 1):
             nt += text[i]
         text = nt
         
     d = True
         
     for x in equipes:
        for i in range(min(len(text),len(x[0]))):
            if text[i].upper() != x[0][i].upper():
                d = False
        if d == True:
            n_teams.append(x[0])
        d = True

     team_nat =  existe_nationalite(nat)
     if nat != "" and nat != "-None-":
         nat_liste = [x[0] for x in team_nat]
     else:
         nat_liste=n_teams
         
     for x in range (0,len(n_teams)):
         if  n_teams[x] in nat_liste:
             teams.insert(x+1,n_teams[x])
     return None
    





""""affichage team"""


def team(t,id_equipe):
            """affiche les informations de l'equipe"""


            #INITIALISATION DES VARIABLES GLOBALES

            global  b_j1;
            global  b_j2;
            global  b_j3;
            global  b_j4;
            global  b_j5;
            global b_j1_w
            global b_j2_w
            global b_j3_w
            global b_j4_w
            global b_j5_w
            global logo
            
            global liste_pays
            global liste_pays_abr
            global photo1
            global photo2
            global photo3
            global photo4
            global photo5

            global r
            global c
            global can
            global photo
            global logo

            

            nom = id_equipe 
            
            if t!= "non":
                t.destroy()
            r = Toplevel()

            photo = PhotoImage(file="cs-go-wallpaper.png")
            logo =  PhotoImage(file="equipes/" + nom +".png")


            can =  Canvas(r, width=1024, height=512)
            r.resizable(False,False)
            
            can.pack()
        
        
            nom = '"'+nom+'"'

            #CREATION DES BOUTONS

            an = "2017"
            jour = "06"
            mois = "06"

            liste_pays = ["belgique","bosnie","bresil","canada","danmark","espagne","estonie","finland","france","hongrie","kazakhstan","norvège","pays bas","pologne","portugal","republique cheque","royame unis","russie","slovaquie","suede","ukraine","usa","allemagne","autriche","suisse"]
            liste_pays_abr =["be","bosnia","brazil","ca","den","esp","est","fin","fr","hun","kaza","nor","neth","pol","port","czech","en","ru","slov","swed","ukr","us","ger","aust","switz",]
            
            c = conn.cursor()
            c.execute("""SELECT PSEUDO,JOUEUR.ID FROM joueur , joue_avec ,TEAM WHERE (joueur.id = ID_P AND ID_T = TEAM.id   AND TEAM.nom = """+nom+""" AND DEBUT   < "2017-06-06" AND FIN > "2017-06-06")""")
            noms2=c.fetchall()
            noms = [(x[0],) for x in noms2]

            c.execute("""SELECT nationalite FROM joueur , joue_avec ,TEAM WHERE (joueur.id = ID_P AND ID_T = TEAM.id   AND TEAM.nom = """+nom+""" AND DEBUT   < "2017-06-06" AND FIN > "2017-06-06")""")
            na = c.fetchall()
            
            photo1 = PhotoImage(file ="no_foto.png")
            photo2 = PhotoImage(file ="no_foto.png")
            photo3 = PhotoImage(file ="no_foto.png")
            photo4 = PhotoImage(file ="no_foto.png")
            photo5 = PhotoImage(file ="no_foto.png")

            palma = idpalmares(id_equipe)
                

            palmares = Listbox(r,height = 8, width  =  45)
            palamares_w = can.create_window(875, 230, window=palmares)
            for x in range (len(palma)):
                palmares.insert(x,palma[x][0] +  " rang: " + str(palma[x][1]))
            palma2 = [x[0] +  " rang: " + str(x[1]) for x in palma]

            palmares.bind('<Double-Button-1>', lambda x: competition2(r,palma[palma2.index(palmares.get(palmares.curselection()))][2] ) )





                        
            b_j1 = Button(r,text="")
            b_j2 = Button(r,text="")
            b_j3 = Button(r,text="")
            b_j4 = Button(r,text="")
            b_j5 = Button(r,text="")



            actualiser(r,jour,mois,an,nom)
            entree = Label(r, text= nom)


            entre_w = can.create_window(170, 150, window=entree)


            an_liste = ["2014","2015","2016","2017"]
            var1 = StringVar()
            var1.set("2017")
            an = OptionMenu(r,var1,*an_liste,command=lambda x: actualiser(r,var3.get(),var2.get(),var1.get(),nom))
            an_w = can.create_window( 200, 300, window=an)

    

            mois_liste = ["01","02","03","04","05","06","07","08","09","10","11","12"]
            var2 = StringVar()
            var2.set("06")
            mois = OptionMenu(r,var2,*mois_liste,command=lambda x: actualiser(r,var3.get(),var2.get(),var1.get(),nom))
            mois_w = can.create_window(120, 300, window=mois)


            jours_liste = ["01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31"]
            var3 = StringVar()
            var3.set("06")
            jour= OptionMenu(r,var3,*jours_liste,command=lambda x: actualiser(r,var3.get(),var2.get(),var1.get(),nom))
            jour_w = can.create_window(40, 300, window=jour)
            r.  mainloop()

            
def actualiser(t,jour,mois,an,nom):
    """Actualise les équipes pour redonner la lineup  a la date X"""

    #Re initialisation des variables globales

    global  b_j1;
    global  b_j2;
    global  b_j3;
    global  b_j4;
    global  b_j5;
    global b_j1_w
    global b_j2_w
    global b_j3_w
    global b_j4_w
    global b_j5_w

    global r
    global c
    global can

    global liste_pays
    global liste_pays_abr

    global photo
    global photo1
    global photo2
    global photo3
    global photo4
    global photo5

    global logo
    
    can.create_image(0,0, anchor=NW, image = photo)
    can.create_image(462 ,75, anchor=NW, image = logo)
  
    
    date = '"' + str(an)+ "-" + str(mois) + "-" + str(jour) + '"'


    
    commande =   """SELECT PSEUDO, JOUEUR.id FROM joueur , joue_avec ,TEAM WHERE (joueur.id = ID_P AND ID_T = TEAM.id   AND TEAM.nom = """+nom+""" AND DEBUT   <""" + date+ """ AND FIN > """ + date + """)"""
    c.execute(commande)
    noms2 = c.fetchall()
    noms = [(x[0],) for x in noms2]
    noms_id = [ x[1] for x in noms2]

    #FAIT LE LIEN ENTRE LA FENETRE D'INFO DE L'EQUIPE AVEC CELLE DU JOUEUR
    def click():
        joueurs(t,noms_id[0])
    def click2():
        joueurs(t,noms_id[1])
    def click3():
        joueurs(t,noms_id[2])
    def click4():
        joueurs(t,noms_id[3])
    def click5():
        joueurs(t,noms_id[4]) 
    c.execute("""SELECT nationalite FROM joueur , joue_avec ,TEAM WHERE (joueur.id = ID_P AND ID_T = TEAM.id   AND TEAM.nom = """+nom+""" AND DEBUT   <""" + date+ """ AND FIN > """ + date + """)""")
    na = c.fetchall()
        
    #RECREATION DES BOUTONS
    
    b_j1.destroy()
    b_j2.destroy()
    b_j3.destroy()
    b_j4.destroy()
    b_j5.destroy()

    if len(noms) == 5:
        b_j1 = Button(r,text=noms[0][0],command=click)
        b_j2 = Button(r,text=noms[1][0],command=click2)
        b_j3 = Button(r,text=noms[2][0],command=click3)
        b_j4 = Button(r,text=noms[3][0],command=click4)
        b_j5 = Button(r,text=noms[4][0],command=click5)

        b_j1_w = can.create_window(225, 475, window=b_j1)
        b_j2_w = can.create_window(375, 475, window=b_j2)
        b_j3_w = can.create_window(525, 475, window=b_j3)
        b_j4_w = can.create_window(675, 475, window=b_j4)
        b_j5_w = can.create_window(825, 475, window=b_j5)


        photo1 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[0][0].lower())]+"_f.png")
        photo2 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[1][0].lower())]+"_f.png")
        photo3 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[2][0].lower())]+"_f.png")
        photo4 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[3][0].lower())]+"_f.png")
        photo5 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[4][0].lower())]+"_f.png")

        can.create_image(175,350, anchor=NW, image = photo1)
        can.create_image(325,350, anchor=NW, image = photo2)
        can.create_image(475,350, anchor=NW, image = photo3)
        can.create_image(625,350, anchor=NW, image = photo4)
        can.create_image(775,350, anchor=NW, image = photo5)

   
    elif len(noms) == 4:
        
        b_j1 = Button(r,text=noms[0][0],command=click)
        b_j2 = Button(r,text=noms[1][0],command=click2)
        b_j3 = Button(r,text=noms[2][0],command=click3)
        b_j4 = Button(r,text=noms[3][0],command=click4)

        b_j1_w = can.create_window(225, 475, window=b_j1)
        b_j2_w = can.create_window(375, 475, window=b_j2)
        b_j3_w = can.create_window(525, 475, window=b_j3)
        b_j4_w = can.create_window(675, 475, window=b_j4)


        photo1 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[0][0].lower())]+"_f.png")
        photo2 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[1][0].lower())]+"_f.png")
        photo3 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[2][0].lower())]+"_f.png")
        photo4 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[3][0].lower())]+"_f.png")


        can.create_image(175,350, anchor=NW, image = photo1)
        can.create_image(325,350, anchor=NW, image = photo2)
        can.create_image(475,350, anchor=NW, image = photo3)
        can.create_image(625,350, anchor=NW, image = photo4)

        
    elif len(noms) == 3:
        b_j1 = Button(r,text=noms[0][0],command=click)
        b_j2 = Button(r,text=noms[1][0],command=click2)
        b_j3 = Button(r,text=noms[2][0],command=click3)

        b_j1_w = can.create_window(225, 475, window=b_j1)
        b_j2_w = can.create_window(375, 475, window=b_j2)
        b_j3_w = can.create_window(525, 475, window=b_j3)

        photo1 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[0][0].lower())]+"_f.png")
        photo2 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[1][0].lower())]+"_f.png")
        photo3 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[2][0].lower())]+"_f.png")


        can.create_image(175,350, anchor=NW, image = photo1)
        can.create_image(325,350, anchor=NW, image = photo2)
        can.create_image(475,350, anchor=NW, image = photo3)


    elif len(noms) == 2:
        b_j1 = Button(r,text=noms[0][0],command=click)
        b_j2 = Button(r,text=noms[1][0],command=click2)

        b_j1_w = can.create_window(225, 475, window=b_j1)
        b_j2_w = can.create_window(375, 475, window=b_j2)

        photo1 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[0][0].lower())]+"_f.png")
        photo2 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[1][0].lower())]+"_f.png")
        
        can.create_image(175,350, anchor=NW, image = photo1)
        can.create_image(325,350, anchor=NW, image = photo2)

        
    elif len(noms) == 1:
        
        b_j1 = Button(r,text=noms[0][0],command=click)
        b_j1_w = can.create_window(225, 475, window=b_j1)
    
        photo1 = PhotoImage(file ="pays/"+liste_pays[liste_pays_abr.index(na[0][0].lower())]+"_f.png")
        can.create_image(175,350, anchor=NW, image = photo1)
    return None






"""recherche competition"""


def competition():
    """permet de rechercher une competition"""
    
    conn = sqlite3.connect("PROJET.sqlite")
    
    c = conn.cursor()
    nom  = "nom"
    c.execute("""SELECT id FROM COMPETITION""")
    competitions_id = c.fetchall()
    c.execute("""SELECT nom FROM COMPETITION""")
    competitions = c.fetchall()
 
    t=Toplevel()

    """ CREATION DES FILTRES """
    
    can2 = Canvas(t, width=1024, height=512)
    photo2 = PhotoImage(file="competitions.png")
    can2.pack()
    t.resizable(False,False)
    a=Button(t,text='retour',command=t.destroy)
    can2.create_image(0, 0, anchor=NW, image=photo2)
    a.pack()
  
    competition= Listbox(t,height = 22, width  =  50)
    for x in range (0,len(competitions)):
        competition.insert(x+1,competitions[x][0])
    
    competition_w = can2.create_window(300, 250, window=competition)

    competition.bind('<Double-Button-1>', lambda x: competition2(t,competitions_id[competitions.index((competition.get(competition.curselection()),))][0]))

    c.execute("""SELECT """ +nom+ """ FROM TEAM""")
    equipes = c.fetchall()
    team = ["None"]
    for x in equipes:
        team.append(x[0])
        

    var1 = StringVar()
    var1.set("None")
    var2 = StringVar()
    var2.set("None")
    var3 = StringVar()
    var3.set("None")
    var4 = StringVar()
    var4.set("None")    
    var5 = StringVar()
    var5.set("None") 
    var6 = StringVar()
    var6.set("None") 
    var7 = StringVar()
    var7.set("None") 
    var8 = StringVar()
    var8.set("None") 
    var9 = StringVar()
    var9.set("None") 


    #action a faire lors d'une interaction avec un bouton       
    def click():
        actualisercompetition([var1.get(),var2.get(),var3.get(),var4.get(),var5.get(),var6.get(),var7.get()],competition,competitions)
       
                 
    equipe1 =  OptionMenu(t,var1,*team,command=lambda x:click())
    equipe1_w = can2.create_window(835, 200, window=equipe1)

    equipe2 =  OptionMenu(t,var2,*team,command=lambda x:click())
    equipe2_w = can2.create_window(835, 240, window=equipe2)

    equipe3 =  OptionMenu(t,var3,*team,command=lambda x:click())
    equipe3_w = can2.create_window(835, 280, window=equipe3)

    equipe4 =  OptionMenu(t,var4,*team,command=lambda x:click())
    equipe4_w = can2.create_window(835, 320, window=equipe4)

    equipe5 =  OptionMenu(t,var5,*team,command=lambda x:click())
    equipe5_w = can2.create_window(835, 360, window=equipe5)

    equipe6 =  OptionMenu(t,var6,*team,command=lambda x:click())
    equipe6_w = can2.create_window(835, 400, window=equipe6)


    equipe7 =  OptionMenu(t,var7,*team,command=lambda x:click())
    equipe7_w = can2.create_window(835, 440, window=equipe7)

    equipe8 =  OptionMenu(t,var8,*team,command=lambda x:click())
    equipe8_w = can2.create_window(835, 480, window=equipe8)
    
    t.mainloop()
    

    
def actualisercompetition(equipe_l,competition,competitions):

        def appart_team(equipe,x):
             equipe2 = []
             if equipe == []:
                equipe2 = a_participe_comp(x)

             else:
                for x in a_participe_comp(x):
                    if x in equipe:
                        equipe2.append(x)
             return equipe2

        equipe =[]
        vide = True
        for x in equipe_l:
            if  x != "None":
                vide = False
                equipe = appart_team(equipe,x)
        if vide == True:
            equipe = [x[0] for x in competitions]
            
        competition.delete(0,len(competitions))
        for i in range(0,len(equipe)):
            competition.insert(i+1,equipe[i])


"""affichge competition"""
            
def competition2(t,id_c):
    t.destroy()
    competitionid = id_c
    conn = sqlite3.connect("PROJET.sqlite")
    c = conn.cursor()
    p = (competitionid,)
    
    c.execute("""SELECT nom FROM COMPETITION WHERE ? = COMPETITION.ID""",p)
    competitions = c.fetchall()
    nom = competitions[0][0]
    c.execute("""SELECT TEAM.NOM,RECOMPENSE,TEAM.ID,A_PARTICIPE.RANG FROM TEAM, A_PARTICIPE,COMPETITION WHERE ? = COMPETITION.ID AND COMPETITION.ID = ID_COMP AND ID_TEAM = TEAM.ID and A_PARTICIPE.RANG = 5""",p)
    quartdefinalistes = c.fetchall()
    c.execute("""SELECT TEAM.NOM,RECOMPENSE,TEAM.ID,A_PARTICIPE.RANG FROM TEAM, A_PARTICIPE,COMPETITION WHERE ? = COMPETITION.ID AND COMPETITION.ID = ID_COMP AND ID_TEAM = TEAM.ID and A_PARTICIPE.RANG = 3""",p)
    demisfinalistes = c.fetchall()
    c.execute("""SELECT TEAM.NOM,RECOMPENSE,TEAM.ID,A_PARTICIPE.RANG FROM TEAM, A_PARTICIPE,COMPETITION WHERE ? = COMPETITION.ID AND COMPETITION.ID = ID_COMP AND ID_TEAM = TEAM.ID and A_PARTICIPE.RANG = 2""",p)
    finaliste = c.fetchall()
    c.execute("""SELECT TEAM.NOM,RECOMPENSE,TEAM.ID,A_PARTICIPE.RANG FROM TEAM, A_PARTICIPE,COMPETITION WHERE ? = COMPETITION.ID AND COMPETITION.ID = ID_COMP AND ID_TEAM = TEAM.ID and A_PARTICIPE.RANG = 1""",p)
    winner = c.fetchall()
    c.execute("""SELECT DATE FROM COMPETITION WHERE COMPETITION.ID = ?""",p)
    date = c.fetchall()

    t=Toplevel()
    can2 = Canvas(t, width=1024, height=512)
    photo2 = PhotoImage(file="competition_m.png")
    can2.pack()
    t.resizable(False,False)
    a=Button(t,text='retour',command=t.destroy)
    can2.create_image(0, 0, anchor=NW, image=photo2)

    nom_l = Label(t, text= nom,font = "Verdana 10 bold")
    nom_w =  can2.create_window(512, 40, window=nom_l)

    date_l = Label(t, text= date[0][0],font = "Verdana 10 bold")
    nom_w =  can2.create_window(900, 470, window=date_l)


    def clickquart1():
        team(t,quartdefinalistes[0][0])
    def clickquart2():
        team(t,quartdefinalistes[1][0])
    def clickquart3():
        team(t,quartdefinalistes[2][0])
    def clickquart4():
        team(t,quartdefinalistes[3][0]) 

    def clickdemis1():
        team(t,demisfinalistes[0][0])
    def clickdemis2():
        team(t,demisfinalistes[1][0])

    def clickfinale():
        team(t,finaliste[0][0])

    def clickwinner():
        team(t,winner[0][0])
    
    


    quart1=Button(t,text=quartdefinalistes[0][0]+'\n'+str(quartdefinalistes[0][1])+"$"+'\n'+str(quartdefinalistes[0][3])+"eme",command = clickquart1)
    quar1_w = can2.create_window(170, 160, window=quart1)

    quart2=Button(t,text=quartdefinalistes[1][0]+'\n'+str(quartdefinalistes[1][1])+"$"+'\n'+str(quartdefinalistes[1][3])+"eme",command = clickquart2)
    quar2_w = can2.create_window(170, 240, window=quart2)
    
    quart3=Button(t,text=quartdefinalistes[2][0]+'\n'+str(quartdefinalistes[2][1])+"$"+'\n'+str(quartdefinalistes[2][3])+"eme",command = clickquart3)
    quar3_w = can2.create_window(170, 320, window=quart3)
    
    quart4=Button(t,text=quartdefinalistes[3][0]+'\n'+str(quartdefinalistes[3][1])+"$"+'\n'+str(quartdefinalistes[3][3])+"eme",command = clickquart4)
    quar4_w = can2.create_window(170, 400, window=quart4)
    
    demis1=Button(t,text=demisfinalistes[0][0]+'\n'+str(demisfinalistes[0][1])+"$"+'\n'+str(demisfinalistes[0][3])+"eme",command = clickdemis1)
    demis1_w = can2.create_window(425, 200, window=demis1)
    
    demis2=Button(t,text=demisfinalistes[1][0]+'\n'+str(demisfinalistes[1][1])+"$"+'\n'+str(demisfinalistes[1][3])+"eme",command = clickdemis2)
    demis2_w = can2.create_window(425, 360, window=demis2)
    
    finale=Button(t,text=finaliste[0][0]+'\n'+str(finaliste[0][1])+"$"+'\n'+str(finaliste[0][3])+"eme",command = clickfinale)
    finale_w = can2.create_window(600, 280, window=finale)
    
    gagnant= Button(t,text=winner[0][0]+'\n'+str(winner[0][1])+"$"+'\n'+str(winner[0][3])+"er",command = clickwinner)
    gagnant_w = can2.create_window(820, 280, window=gagnant)

    t.mainloop()
        




"""affichage joueur"""




def joueurs(t,id_p):

     t.destroy()
     r = Toplevel()
     photo = PhotoImage(file="joueur.png")     
     can =  Canvas(r, width=1024, height=512)
     can.create_image(0, 0, anchor=NW, image=photo)
     r.resizable(False,False)
     can.pack()
     nom = infos_joueur(id_p)[0][1]
     prenom = infos_joueur(id_p)[0][2]
     age = infos_joueur(id_p)[0][4]
     na = infos_joueur(id_p)[0][3]
     pseudo = infos_joueur(id_p)[0][0]
     if equipe_act(id_p) != []:
         equipe = equipe_act(id_p)[0][0]
     else:
         equipe = "pas d'équipe"

     
     
     nom_l = Label(r, text= nom,fg = "red",font = "Verdana 10 bold")
     prenom_l = Label(r, text= prenom,fg = "red",font = "Verdana 10 bold")
     equipe_l = Label(r, text= equipe,fg = "red",font = "Verdana 10 bold")
     age_l = Label(r, text= age,fg = "red",font = "Verdana 10 bold")
     na_l = Label(r, text = na,fg = "red",font = "Verdana 10 bold")
     pseudo_l = Label(r, text = pseudo,fg = "red",font = "Verdana 10 bold")

     def click():
          if equipe!="pas d'équipe":
                  team(r,equipe)
          
     b_equipe = Button(r,text=equipe,command = click)
     b_equipe_w = can.create_window(650, 325, window=b_equipe)
     

     nom_w = can.create_window(300, 90, window=nom_l)
     prenom_w = can.create_window(300, 115, window=prenom_l)
     age_w = can.create_window(300, 140, window=age_l)
     na_w = can.create_window(300, 165, window=na_l)
     pseudo_w = can.create_window(800,80, window=pseudo_l)

     histo = a_jouer_avec(id_p)
     histo_equipe_l = [x[0] for x in histo]
     histo_equipe = Listbox(r,height = 4, width  =  25)
     for i in range(len( histo_equipe_l)):
         histo_equipe.insert(i+1, histo_equipe_l[i])
     histo_equipe_w = can.create_window(200, 350, window=histo_equipe)
     histo_equipe.bind('<Double-Button-1>', lambda x: team(r,histo_equipe.get(histo_equipe.curselection())) )



     
     r.mainloop()


    
def recherchejoueurs():
    conn = sqlite3.connect("PROJET.sqlite")
    
    c = conn.cursor()
    nom  = "nom"
    c.execute("""SELECT id FROM JOUEUR""")
    pseudos = c.fetchall()
    
    t=Toplevel()
    can2 = Canvas(t, width=1024, height=512)
    photo2 = PhotoImage(file="recherche_players.png")
    can2.pack()
    t.resizable(False,False)
    a=Button(t,text='retour',command=t.destroy)
    can2.create_image(0, 0, anchor=NW, image=photo2)
    a.pack()
    nationalite = ["-None-","fr","be","czech","ger","en","us","swed","aust","ru","brazil","fin","bosnia","pol","ukr","ca","neth","est","den","nor","esp","kaza","slov","hun","port","switz",]
    team = ["-None-","Astralis","CLG","dignitas","Faze Clan","FlipSid3 Tactics","fnatic","G2_E-sport","Gambit","Hellraisers","Keyd Stars","Kinguin","LDLC.com","Luminosity Gaming","Natus_Vincere","Ninjas_in_pyjamas","North","Penta_E-sport","SK Gaming","Team EnVyUS","Team Liquid","Team SoloMid","Virtus.pro"]

  
    players = Listbox(t,height = 22, width  =  50)
    joueurs_pseudo = []
    for x in range (0,len(pseudos)):
        nom = infos_joueur(pseudos[x][0])[0][0]
        while nom in  joueurs_pseudo :
            nom+="*"
        joueurs_pseudo.append(nom)
        players.insert(x+1,joueurs_pseudo[x])

    players.bind('<Double-Button-1>', lambda x: joueurs(t,joueurs_pseudo.index(players.get(players.curselection()))+1) )

    
    
    players_w = can2.create_window(300, 250, window=players)
    var1 = StringVar()


    
    var2 = StringVar() 
    
         
    nom_var = StringVar()
    prenom_var = StringVar()
    pseudo_var = StringVar()
    age_var = StringVar()

    nom = Entry(t, textvariable= nom_var, width=15)
    prenom = Entry(t, textvariable= prenom_var, width=15)
    pseudo = Entry(t, textvariable= pseudo_var, width=15)
    age_str =  Entry(t, textvariable= age_var, width=3)

    nom_w = can2.create_window(780, 130, window=nom)
    prenom_w = can2.create_window(780, 180, window=prenom)
    pseudo_w = can2.create_window(780, 80, window=pseudo)
    age_w = can2.create_window(730, 235, window=age_str)
    
    def click(key):
        test = True
        if  key.keycode  == 8:	
            test = False
        actualiserjoueurs(players,pseudos,nom_var.get(),prenom_var.get(),pseudo_var.get(),age_var.get(),key.char,test,"pseudo",var1.get(),var2.get())


    def click2(key):
        test = True
        if  key.keycode  == 8:	
            test = False
        actualiserjoueurs(players,pseudos,nom_var.get(),prenom_var.get(),pseudo_var.get(),age_var.get(),key.char,test,"nom",var1.get(),var2.get())


    def click3(key):
        test = True
        if  key.keycode  == 8:	
            test = False
        actualiserjoueurs(players,pseudos,nom_var.get(),prenom_var.get(),pseudo_var.get(),age_var.get(),key.char,test,"prenom",var1.get(),var2.get())


    def click4(key):
        test = True
        if  key.keycode  == 8:	
            test = False
        actualiserjoueurs(players,pseudos,nom_var.get(),prenom_var.get(),pseudo_var.get(),age_var.get(),key.char,test,"age",var1.get(),var2.get())

    def click5():
        actualiserjoueurs(players,pseudos,nom_var.get(),prenom_var.get(),pseudo_var.get(),age_var.get(),"",False,"age",var1.get(),var2.get())


    nationalite_l = OptionMenu(t,var1,*nationalite,command = lambda x: click5())
    nationalite_w = can2.create_window(870, 235, window=nationalite_l)
    team_l = OptionMenu(t,var2,*team,command = lambda x: click5())
    team_w = can2.create_window(750, 300, window=team_l)

        
    pseudo.bind("<Key>", click)
    nom.bind("<Key>", click2)
    prenom.bind("<Key>", click3)
    age_str.bind("<Key>", click4)


    t.mainloop()



        
    


def actualiserjoueurs(players,pseudos,nom,prenom,pseudo,age,key,test,mod,nat,equipe):

     players.delete(0,len(pseudos))
     n_players = []
     aged = 0
     agef = 1000
     if mod == "age" and test == True:
          age = age + key

     if len(age) != 0:
          aged = int(age)
          agef = int(age)
          
     

     
     if test == True and mod == "pseudo":
        pseudo = pseudo + key
     
     elif test == False and mod == "pseudo":
         nt = ""
         for i in range(len(pseudo) - 1):
             nt += pseudo[i]
         pseudo = nt


     if test == True and mod == "nom":
        nom = nom + key
     
     elif test == False and mod == "nom":
         nt = ""
         for i in range(len(pseudo) - 1):
             nt += pseudo[i]
         nom = nt

     if test == True and mod == "prenom":
        prenom = prenom + key
     
     elif test == False and mod == "prenom":
         nt = ""
         for i in range(len(pseudo) - 1):
             nt += pseudo[i]
         prenom = nt

     d = True

     if pseudo != "":
          pseudos_l = pseudo_commence(pseudo)
     else:
          pseudos_l = pseudos

     if nom != "":
          nom_l = nom_commence(nom)
     else:
          nom_l = pseudos

     if prenom != "":
          prenom_l = prenom_commence(prenom)
     else:
          prenom_l = pseudos

     if age != "":
          age_l = de_age(int(age))
     else:
          age_l = pseudos

     if equipe != "" and equipe !=  "-None-":
          equipe_l = a_jouerdans(equipe)
     else:
          equipe_l = pseudos

     if nat != "" and nat != "-None-":
           nat_l = idjoueur_nationalite(nat)
     else:
           nat_l = pseudos
          
     for x in range (0,len(pseudos)):
         if pseudos[x] in pseudos_l and pseudos[x] in nom_l and pseudos[x] in prenom_l  and pseudos[x] in age_l  and pseudos[x] in equipe_l and pseudos[x] in nat_l:
              players.insert(x+1,infos_joueur(pseudos[x][0])[0][0])
     return None
    


""""corps principal"""


w = Tk()
w.resizable(False,False)
canard = Canvas(w, width=1024, height=512, bg="white")
photocan = PhotoImage(file="csgo-logo.png")
canard.pack()


canard.create_image(0, 0, anchor=NW, image=photocan)
bt_team = Button(w, text=" o ", command = rechercheteam)
bt_team_w = canard.create_window(110, 177, window=bt_team)


bt_joueur = Button(w, text=" o ",command = recherchejoueurs)
bt_joueur_w = canard.create_window(110, 205, window=bt_joueur)

bt_comp = Button(w, text=" o ", command = competition)
bt_comp_w = canard.create_window(160, 242, window=bt_comp)


bt_quitter = Button(w, text=" o ", command = quit)
bt_quitter_w = canard.create_window(950, 460, window=bt_quitter)

rank = []
rang = equipe_par_rang()

for x in rang:
    if x[1] !=0:
        rank.append(x)
rang = rank

rank = []
rang2 = []
for x in range( len(rang)):
    rank.append(str(rang[x][1])+": "+rang[x][0])


liste = Listbox(w)
for x in range (0,len(rank)):
    liste.insert(x+1,rank[x])
    rang2.append(rank[x])

liste.bind('<Double-Button-1>', lambda x: team("non",rang[  rang2.index(liste.get(liste.curselection()),)][0]))

bt_rank_w = canard.create_window(70, 420, window=liste)
w.mainloop()



    
