# TIPE-2018
Compilation, application à Scheme->forth et tout et tout.

## stf/
Compilateur pas à jour construit avec les outils fournis par caml: camllex et camlyacc.
## homemade/
Compilateur from-scratch très stylé.
### Compilation
1) Se placer dans homemade/
2) Lancer make
### Utilisation
hmstf -i infile -o outfile.
- infile vaut in.scheme si non spécifé.
- outfile vaut out.forth si non spécifé.
### Petites features
Note: on ne travaille pour l'instant qu'avec des entiers non signés.
- Création et manipulation de variables.
- Opérations arithmétiques
- ...

# Note
Ca ne fonctionne que sous linux hein...
