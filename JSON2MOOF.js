import { JSON2Atom, Atoms, AtomsJSON, WriteAtoms } from './mp4.js'

const
fs = await import( 'fs' )

const
moof = JSON2Atom( JSON.parse( fs.readFileSync( 0 ) ) )

const
O = new Uint8Array( moof.Size() )

const size =
WriteAtoms( [ moof ], O )
console.error( moof.Size(), size )

fs.writeFileSync( 1, O )

