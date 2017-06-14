from rdkit.Chem import AllChem
from rdkit.Chem import Draw
import sys
print 'Number of arguments:', len(sys.argv), 'arguments.'
print 'Argument List:', str(sys.argv)
print sys.argv[1]
#smiles = "CN1C=NC2=C1C(=O)N(C(=O)N2C)C"
smiles = sys.argv[1]
file=sys.argv[2]
x=int(sys.argv[3])
y=int(sys.argv[4])
print smiles
mol = AllChem.MolFromSmiles(smiles)
Draw.MolToFile(mol,file,size=(x,y))

