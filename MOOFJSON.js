import { Atoms, AtomsJSON, WriteAtoms } from './mp4.js'

const
fs = await import( 'fs' )

const
I = new Uint8Array( fs.readFileSync( 0 ) )

const
moof = AtomsJSON( Atoms( I ) )[ 0 ]

console.log( JSON.stringify( moof ) )

