import sqlite3 
conn = sqlite3.connect("PROJET.sqlite")

def equipe_par_rang () :
    """Donne la liste des équipes de rang 1 à 22 rangées par ordre croissant"""
    c=conn.cursor()
    c.execute ("""SELECT nom , rang FROM TEAM ORDER BY rang ASC""")
    r=c.fetchall()
    return r


def existe_nationalite (nat) :
    """Donne la liste des équipes dans
    lesquelles il y a actuellement au moins un joueur de la nationalité nat"""
    c=conn.cursor()
    p = (nat, )
    c.execute("""SELECT DISTINCT nom FROM (SELECT TEAM.nom , ID_P FROM TEAM JOIN joue_avec ON TEAM.ID=ID_T WHERE FIN = "2020-01-01" INTERSECT SELECT TEAM.nom , ID_P FROM TEAM JOIN joue_avec ON TEAM.ID=ID_T , JOUEUR WHERE ID_P=JOUEUR.id AND UPPER (nationalite)=UPPER (?))""", p )
    r=c.fetchall()
    return r

def idpalmares(id_equipe):
    """Donne le palmarès de l'équipe dont l'identifiant est donné en argument"""
    c=conn.cursor()
    p = (id_equipe, )
    c.execute("""SELECT COMPETITION.nom, A_PARTICIPE.rang,COMPETITION.ID FROM COMPETITION, TEAM, A_PARTICIPE WHERE TEAM.id = ID_TEAM AND competition.id = ID_COMP AND TEAM.NOM = ?""", p)
    r=c.fetchall()
    return r

# Requete renvoyant les identifiants des joueurs en donnant un critère en argument.
# Fonctions de la forme idjoueur_critère (valeur_critère)               
  
def idjoueur_nom (nom):
    """Renvoie les identifiants des joueurs dont on a donné le nom"""
    c = conn.cursor()
    p = (nom, )
    c.execute("""SELECT id FROM JOUEUR WHERE UPPER(nom) = UPPER(?)""", p)
    r=c.fetchall()
    return r

def idjoueur_prenom (prenom):
    """Renvoie les identifiants des joueurs dont on a donné le prénom"""
    c = conn.cursor()
    p = (prenom, )
    c.execute("""SELECT id FROM JOUEUR WHERE UPPER(prenom) = UPPER(?)""", p)
    r=c.fetchall()
    return r

def idjoueur_nationalite (nationalite):
    """Renvoie les identifiants des joueurs dont on a donné la nationalité"""
    c = conn.cursor()
    p = (nationalite, )
    c.execute("""SELECT id FROM JOUEUR WHERE UPPER(nationalite) = UPPER(?)""", p)
    r=c.fetchall()
    return r

def idjoueur_pseudo (pseudo):
    """Renvoie l'identifiant du joueur dont on a donné le pseudo"""
    c = conn.cursor()
    p = (pseudo, )
    c.execute("""SELECT id FROM JOUEUR WHERE UPPER(pseudo) = UPPER(?)""", p)
    r=c.fetchall()
    return r

def idjoueur_equipe (equipe) :
    """Renvoie les identifiants des joueurs qui ont joué dans l'équipe dont on a donné l'identifiant"""
    c = conn.cursor()
    p = (equipe, )
    c.execute("""SELECT JOUEUR.id FROM JOUEUR , joue_avec, TEAM WHERE ID_T =TEAM.id AND JOUEUR.id = ID_P AND TEAM.nom= ? """, p)
    r=c.fetchall()
    return r


# Requete renvoyant les informations générales sur l'objet demandé.
# De la forme infos_objet (idobjet).

def infos_joueur (idjoueur) :
    """Renvoie le triplet (pseudo,nom,prenom,nationalité,age) du joueur dont on a donné l'identifiant"""
    c=conn.cursor()
    p = (idjoueur, )
    c.execute("""SELECT pseudo, nom, prenom, nationalite, age FROM JOUEUR WHERE id= ? """, p)
    r=c.fetchall()
    return r

def a_participe (idcompet):
    """renvoie la liste des equipes ayant participer a la competition"""
    c=conn.cursor()
    p = (idcompet, )
    c.execute("""SELECT id FROM TEAM,A_PARTICIPE WHERE id = ID_TEAM AND ID_COMP = ?""", p)
    r=c.fetchall()
    x =[]
    for i in r:
        x.append(i[0])
    return x

def nom_equipes(id_equipe):
    """Selectionne toute les equipes"""
    c=conn.cursor()
    p = (id_equipe, )
    c.execute("""SELECT nom FROM TEAM WHERE id = ?""", p)
    r=c.fetchall()
    return r[0][0]

def a_jouerdans(nom_equipe):
    """selectione tous les joueurs ayant un jour jouer dans la team"""
    c=conn.cursor()
    p = (nom_equipe, )
    c.execute("""SELECT DISTINCT JOUEUR.id FROM JOUEUR, JOUE_AVEC,TEAM WHERE TEAM.nom = ? AND TEAM.ID = ID_T AND JOUEUR.ID = ID_P""", p)
    r=c.fetchall()
    return r

def a_participe_comp (nom_equipe):
    """renvoie toutes les competitions faisant partie du Palmares de l'equipe"""
    c=conn.cursor()
    p = (nom_equipe, )
    c.execute("""SELECT COMPETITION.NOM FROM TEAM,COMPETITION,A_PARTICIPE WHERE TEAM.id = ID_TEAM AND TEAM.NOM  = ? AND COMPETITION.ID = ID_COMP""", p)
    r=c.fetchall()
    x =[]
    for i in r:
        x.append(i[0])
    return x


def pseudo_commence (lettre):
    """selectione les joueurs dont le pseudo commence par lettre"""
    c=conn.cursor()
    lettre = "'"+lettre+"%'"
    c.execute("""SELECT id FROM JOUEUR WHERE PSEUDO LIKE""" + lettre)
    r=c.fetchall()
    return r
def nom_commence (lettre):
    """selectione les joueurs dont le nom commence par lettre"""
    c=conn.cursor()
    lettre = "'"+lettre+"%'"
    c.execute("""SELECT id FROM JOUEUR WHERE NOM LIKE""" + lettre)
    r=c.fetchall()
    return  r

def prenom_commence (lettre):
    """selectione les joueurs dont le prenom commence par lettre"""
    c=conn.cursor()
    lettre = "'"+lettre+"%'"
    c.execute("""SELECT id FROM JOUEUR WHERE PRENOM LIKE""" + lettre)
    r=c.fetchall()
    return r

def de_age (age):
    """selectione les joueurs du bon age"""
    c=conn.cursor()
    p = (age, )
    c.execute("""SELECT id FROM JOUEUR WHERE AGE =?""",p)
    r=c.fetchall()
    return r


def equipe_act (id_p):
    """renvoie l equipe actuelle du joueur"""
    c=conn.cursor()
    p = (id_p, )
    c.execute("""SELECT TEAM.NOM FROM TEAM,JOUE_AVEC WHERE ID_P =? AND ID_T = TEAM.ID AND DEBUT <= "2017-06-06" AND FIN >= "2020-01-01"  """,p)
    r=c.fetchall()
    return r

def a_jouer_avec (id_p):
    """renvoie la liste des anciennes equipes du joueur"""
    c=conn.cursor()
    p = (id_p, )
    c.execute("""SELECT DISTINCT TEAM.NOM FROM TEAM,JOUE_AVEC WHERE ID_P =? AND ID_T = TEAM.ID""",p)
    r=c.fetchall()
    return r


