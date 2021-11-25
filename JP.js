export const 
MakeDataView = ( typed, offset = 0 ) => new DataView(
	typed.buffer
,	typed.byteOffset + offset
,	typed.byteLength - offset
)

export const
SubBytes = ( bytes, $, _ ) => new Uint8Array(
	bytes.buffer
,	bytes.byteOffset + $
,	_ === void 0 ? bytes.length - $ : _
)
