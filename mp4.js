import { MakeDataView, SubBytes } from './JP.js'

const
TimeString_MP4TimeStamp = $ => new Date( $ - 1000 * 3600 * 24 * ( 365 * 66 + 17 ) )
//TimeString_MP4TimeStamp = $ => $

const
HexChar = $ => {
	switch ( $ ) {
	case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7: case 8: case 9:
		return $ + 0x30
	case 10: case 11: case 12: case 13: case 14: case 15:
		return $ - 10 + 0x61
	default:	throw "eh?"
	}
}

const
HexStr = byte => String.fromCharCode( HexChar( byte >> 4 ), HexChar( byte & 0x0f ) )

const
EncodeHex = bytes => bytes.reduce( ( $, _ ) => $ + HexStr( _ ), '' )

const
EncodeHexLF16 = bytes => Array.from(
	{ length: Math.floor( bytes.length + 15 ) / 16 }
,	( $, _ ) => {
		_ *= 16
		const nRemain = bytes.length - _
		return EncodeHex( SubBytes( bytes, _, _ + ( nRemain < 16 ? nRemain : 16 ) ) )
	}
)

const
TypeString = $ => String.fromCharCode( $ >> 24, $ >> 16 & 0x0ff, $ >> 8 & 0x0ff, $ & 0x0ff )

const
QUARTET = s => ( s.charCodeAt( 0 ) << 24 | s.charCodeAt( 1 ) << 16 | s.charCodeAt( 2 ) << 8 | s.charCodeAt( 3 ) )

const Q_ftyp = QUARTET( 'ftyp' )
const Q_styp = QUARTET( 'styp' )
const Q_mvhd = QUARTET( 'mvhd' )
const Q_coin = QUARTET( 'coin' )
const Q_hdlr = QUARTET( 'hdlr' )
const Q_xml  = QUARTET( 'xml ' )
const Q_iloc = QUARTET( 'iloc' )
const Q_idat = QUARTET( 'idat' )
const Q_tkhd = QUARTET( 'tkhd' )
const Q_elst = QUARTET( 'elst' )
const Q_mdhd = QUARTET( 'mdhd' )
const Q_vmhd = QUARTET( 'vmhd' )
const Q_smhd = QUARTET( 'smhd' )
const Q_sthd = QUARTET( 'sthd' )
const Q_dref = QUARTET( 'dref' )
const Q_frma = QUARTET( 'frma' )
const Q_schm = QUARTET( 'schm' )
const Q_tenc = QUARTET( 'tenc' )
const Q_stts = QUARTET( 'stts' )
const Q_stsc = QUARTET( 'stsc' )
const Q_stsz = QUARTET( 'stsz' )
const Q_stz2 = QUARTET( 'stz2' )
const Q_stco = QUARTET( 'stco' )
const Q_mehd = QUARTET( 'mehd' )
const Q_trex = QUARTET( 'trex' )
const Q_pssh = QUARTET( 'pssh' )
const Q_free = QUARTET( 'free' )
const Q_sidx = QUARTET( 'sidx' )
const Q_emsg = QUARTET( 'emsg' )
const Q_mfhd = QUARTET( 'mfhd' )
const Q_tfhd = QUARTET( 'tfhd' )
const Q_tfdt = QUARTET( 'tfdt' )
const Q_trun = QUARTET( 'trun' )
const Q_avcn = QUARTET( 'avcn' )
const Q_senc = QUARTET( 'senc' )
const Q_saio = QUARTET( 'saio' )
const Q_saiz = QUARTET( 'saiz' )
const Q_sbgp = QUARTET( 'sbgp' )
const Q_sgpd = QUARTET( 'sgpd' )
const Q_mdat = QUARTET( 'mdat' )
const Q_tfra = QUARTET( 'tfra' )
const Q_mfro = QUARTET( 'mfro' )

const Q_avc1 = QUARTET( 'avc1' )

const Q_encv = QUARTET( 'encv' )
const Q_avcC = QUARTET( 'avcC' )
const Q_enca = QUARTET( 'enca' )
const Q_mp4a = QUARTET( 'mp4a' )

const Q_moov = QUARTET( 'moov' )
const Q_trak = QUARTET( 'trak' )
const Q_edts = QUARTET( 'edts' )
const Q_mdia = QUARTET( 'mdia' )
const Q_minf = QUARTET( 'minf' )
const Q_dinf = QUARTET( 'dinf' )
const Q_stbl = QUARTET( 'stbl' )
const Q_sinf = QUARTET( 'sinf' )
const Q_schi = QUARTET( 'schi' )
const Q_mvex = QUARTET( 'mvex' )
const Q_moof = QUARTET( 'moof' )
const Q_traf = QUARTET( 'traf' )
const Q_mfra = QUARTET( 'mfra' )
const Q_udta = QUARTET( 'udta' )
const Q_ilst = QUARTET( 'ilst' )
const Q_too  = 0xa9746f6f
const Q_data = QUARTET( 'data' )

const Q_meta = QUARTET( 'meta' )
const Q_stsd = QUARTET( 'stsd' )

const Q_colr = QUARTET( 'colr' )
const Q_pasp = QUARTET( 'pasp' )
const Q_btrt = QUARTET( 'btrt' )
const Q_esds = QUARTET( 'esds' )

class
BitReader {

	constructor( bytes ) {
		this.bytes = bytes
		this.index = 0
	}

	Read() {
		const _ = this.bytes[ Math.floor( this.index / 8 ) ] & ( 0x00000080 >> ( this.index % 8 ) )
		this.index++
		return _ ? 1 : 0
	}

	Skip( $ ) {
		this.index += $
	}

	ReadBits( $ ) {
		let _ = 0
		while ( $-- ) _ = ( _ << 1 ) | this.Read()
		return _
	}

	ReadGolomb() {
		let _ = 0
		while ( !this.Read() ) _++
		return _ ? ( 1 << _ ) - 1 + this.ReadBits( _ ) : 0
	}

	SignedGolomb() {
		const _ = this.ReadGolomb()
		return _ % 2
		?	( _ + 1 ) / 2
		:	-_ / 2
	}
}

const	AVC_SPS_MAX_ID										= 255
const	AVC_SPS_MAX_NUM_REF_FRAMES_IN_PIC_ORDER_CNT_CYCLE	= 256
const	AVC_SPS_MAX_SCALING_LIST_COUNT						= 12
const	AVC_PPS_MAX_ID										= 255
const	AVC_PPS_MAX_SLICE_GROUPS							= 256
const	AVC_PPS_MAX_PIC_SIZE_IN_MAP_UNITS					= 65536

const
AvcSPS = bytes => {


	const $ = {}

	const bits = new BitReader( bytes )
	bits.Skip( 8 ); // NAL Unit Type

	$.profile_idc = bits.ReadBits( 8 )
	$.constraint_set0_flag = bits.Read()
	$.constraint_set1_flag = bits.Read()
	$.constraint_set2_flag = bits.Read()
	$.constraint_set3_flag = bits.Read()
	$.constraint_set4_flag = bits.Read()
	$.constraint_set5_flag = bits.Read()
	bits.Skip( 2 )
	$.level_idc = bits.ReadBits( 8 )
	$.seq_parameter_set_id = bits.ReadGolomb()
	if ( $.seq_parameter_set_id > AVC_SPS_MAX_ID ) throw "ERROR_INVALID_FORMAT"
	if ($.profile_idc  ==  100
	||	$.profile_idc  ==  110
	||	$.profile_idc  ==  122
	||	$.profile_idc  ==  244
	||	$.profile_idc  ==  44
	||	$.profile_idc  ==  83
	||	$.profile_idc  ==  86
	||	$.profile_idc  ==  118
	||	$.profile_idc  ==  128
	||	$.profile_idc  ==  138
	||	$.profile_idc  ==  139
	||	$.profile_idc  ==  134
	) {
		$.chroma_format_idc = bits.ReadGolomb()
		$.separate_colour_plane_flag = 0
		if ($.chroma_format_idc == 3) {
			$.separate_colour_plane_flag = bits.Read();	//	ORG SKIP
		}
		$.bit_depth_luma_minus8 = bits.ReadGolomb()
		$.bit_depth_chroma_minus8 = bits.ReadGolomb()
		$.qpprime_y_zero_transform_bypass_flag = bits.Read();	//	ORG SKIP
		$.seq_scaling_matrix_present_flag = bits.Read()
		if ( $.seq_scaling_matrix_present_flag ) {
			for ( let i = 0; i < ( $.chroma_format_idc != 3 ? 8 : 12 ); i++ ) {
				const seq_scaling_list_present_flag = bits.Read();	//	ORG SKIP
				if (seq_scaling_list_present_flag) {
					const last_scale = 8
					const next_scale = 8
					if ( i < 6 ) {
						for ( let j = 0; j < 16; j++ ) {
							if ( next_scale ) {
								const delta_scale = bits.SignedGolomb()
								next_scale = ( last_scale + delta_scale + 256 ) % 256
								$.use_default_scaling_matrix_4x4[ i ] = ( j == 0 && next_scale == 0 )
							}
							$.scaling_list_4x4[ i ].scale[ j ] = ( next_scale == 0 ? last_scale : next_scale )
							last_scale = $.scaling_list_4x4[ i ].scale[ j ]
						}
					} else {
						for ( let j = 0; j < 64; j++ ) {
							if ( next_scale ) {
								const delta_scale = bits.SignedGolomb()
								next_scale = ( last_scale + delta_scale + 256 ) % 256
								$.use_default_scaling_matrix_8x8[ i - 6 ] = ( j == 0 && next_scale == 0 )
							}
							$.scaling_list_8x8[ i - 6 ].scale[ j ] = ( next_scale == 0 ? last_scale : next_scale )
							last_scale = $.scaling_list_8x8[ i - 6 ].scale[ j ]
						}
					}
				}
			}
		}
	} else {
		$.chroma_format_idc = 1
	}
	$.log2_max_frame_num_minus4 = bits.ReadGolomb()
	$.pic_order_cnt_type = bits.ReadGolomb()
	if ($.pic_order_cnt_type > 2) throw "ERROR_INVALID_FORMAT"
	if ($.pic_order_cnt_type == 0) {
		$.log2_max_pic_order_cnt_lsb_minus4 = bits.ReadGolomb()
	} else if ($.pic_order_cnt_type == 1) {
		$.delta_pic_order_always_zero_flags = bits.Read()
		$.offset_for_non_ref_pic = bits.SignedGolomb()
		$.offset_for_top_to_bottom_field = bits.SignedGolomb()
		$.num_ref_frames_in_pic_order_cnt_cycle = bits.ReadGolomb()
		if ($.num_ref_frames_in_pic_order_cnt_cycle > AVC_SPS_MAX_NUM_REF_FRAMES_IN_PIC_ORDER_CNT_CYCLE) throw "ERROR_INVALID_FORMAT"
		for ( let i=0; i<$.num_ref_frames_in_pic_order_cnt_cycle; i++) {
			$.offset_for_ref_frame[i] = bits.SignedGolomb()
		}
	}
	$.num_ref_frames						= bits.ReadGolomb()
	$.gaps_in_frame_num_value_allowed_flag	= bits.Read()
	$.pic_width_in_mbs_minus1				= bits.ReadGolomb()
	$.pic_height_in_map_units_minus1		= bits.ReadGolomb()
	$.frame_mbs_only_flag					= bits.Read()
	if (!$.frame_mbs_only_flag) {
		$.mb_adaptive_frame_field_flag		= bits.Read()
	}
	$.direct_8x8_inference_flag = bits.Read()
	$.frame_cropping_flag					= bits.Read()
	if ($.frame_cropping_flag) {
		$.frame_crop_left_offset			= bits.ReadGolomb()
		$.frame_crop_right_offset			= bits.ReadGolomb()
		$.frame_crop_top_offset				= bits.ReadGolomb()
		$.frame_crop_bottom_offset			= bits.ReadGolomb()
	}

	return $
}

const
AvcPPS = bytes => {

	const $ = {}

	const bits = new BitReader( bytes )
	bits.Skip( 8 ); // NAL Unit Type

	$.pic_parameter_set_id	 = bits.ReadGolomb()
	if ($.pic_parameter_set_id > AVC_PPS_MAX_ID) throw "ERROR_INVALID_FORMAT"
	$.seq_parameter_set_id	 = bits.ReadGolomb()
	if ($.seq_parameter_set_id > AVC_SPS_MAX_ID) throw "ERROR_INVALID_FORMAT"
	$.entropy_coding_mode_flag	= bits.Read()
	$.pic_order_present_flag	= bits.Read()
	$.num_slice_groups_minus1	= bits.ReadGolomb()
	if ($.num_slice_groups_minus1 >= AVC_PPS_MAX_SLICE_GROUPS) throw "ERROR_INVALID_FORMAT"
	if ($.num_slice_groups_minus1 > 0) {
		$.slice_group_map_type = bits.ReadGolomb()
		if ($.slice_group_map_type == 0) {
			for (let i=0; i<=$.num_slice_groups_minus1; i++) {
				$.run_length_minus1[i] = bits.ReadGolomb()
			}
		} else if ($.slice_group_map_type == 2) {
			for (let i=0; i<$.num_slice_groups_minus1; i++) {
				$.top_left[i] = bits.ReadGolomb()
				$.bottom_right[i] = bits.ReadGolomb()
			}
		} else if ($.slice_group_map_type == 3 ||
				$.slice_group_map_type == 4 ||
				$.slice_group_map_type == 5) {
			$.slice_group_change_direction_flag = bits.Read()
			$.slice_group_change_rate_minus1 = bits.ReadGolomb()
		} else if ($.slice_group_map_type == 6) {
			$.pic_size_in_map_units_minus1 = bits.ReadGolomb()
			if ($.pic_size_in_map_units_minus1 >= AVC_PPS_MAX_PIC_SIZE_IN_MAP_UNITS) throw "ERROR_INVALID_FORMAT"
			let num_bits_per_slice_group_id
			if ($.num_slice_groups_minus1 + 1 > 4) {
				num_bits_per_slice_group_id = 3
			} else if ($.num_slice_groups_minus1 + 1 > 2) {
				num_bits_per_slice_group_id = 2
			} else {
				num_bits_per_slice_group_id = 1
			}
			for (let i=0; i<=$.pic_size_in_map_units_minus1; i++) {
				/*$.slice_group_id[i] =*/ bits.ReadBits( num_bits_per_slice_group_id )
			}
		}
	}
	$.num_ref_idx_10_active_minus1				= bits.ReadGolomb()
	$.num_ref_idx_11_active_minus1				= bits.ReadGolomb()
	$.weighted_pred_flag						= bits.Read()
	$.weighted_bipred_idc						= bits.ReadBits( 2 )
	$.pic_init_qp_minus26						= bits.SignedGolomb()
	$.pic_init_qs_minus26						= bits.SignedGolomb()
	$.chroma_qp_index_offset					= bits.SignedGolomb()
	$.deblocking_filter_control_present_flag	= bits.Read()
	$.constrained_intra_pred_flag				= bits.Read()
	$.redundant_pic_cnt_present_flag			= bits.Read()

	return $
}


export const
AtomsJSON = $ => $.map( $ => $.JSON() )

let
IV_Size

export class
Atom {

	constructor( type, bytes, children ) {
		this.type = type
		this.bytes = bytes
		this.children = children
		this.dataView = MakeDataView( bytes )
	}

	Size() {
		const $ = this.RawSize() + 8
		return $ < 0x100000000 ? $ : $ + 8
	}

	RawSize() {
		return this.bytes.length + this.children.reduce( ( $, _ ) => $ + _.Size(), 0 )
	}

	Write( bytes ) {	//	Assuming this bytes has sufficient memory, so no need to use `length`.
		const
		_		= MakeDataView( bytes )

		_.setUint32( 4, this.type )

		const
		size	= this.Size()
		let
		offset	= size < 0x100000000
		?	(	_.setUint32( 0, size )
			,	8
			)
		:	(	_.setUint32( 0, 1 )
			,	_.setUint64( 8, size )
			,	16
			)
		bytes.set( this.bytes, offset )
		offset += this.bytes.length
		offset += WriteAtoms( this.children, SubBytes( bytes, offset ) )
		return offset
	}

	Hex		( $, _ )	{ return EncodeHex( SubBytes( this.bytes, $, _ ) )		}
	
	HexLF16	( $, _ )	{ return EncodeHexLF16( SubBytes( this.bytes, $, _ ) )	}
	
	Size8	( offset )	{ return Number( this.dataView.getBigUint64( offset ) )	}
	Size4	( offset )	{ return this.dataView.getUint32( offset )		}
	Size2	( offset )	{ return this.dataView.getUint16( offset )		}
	Size1	( offset )	{ return this.dataView.getUint8( offset )		}

	Int8	( offset )	{ return this.dataView.getBigInt64( offset )	}
	Int4	( offset )	{ return this.dataView.getInt32( offset )		}

	JSON() {

		const $ = {
			'META': TypeString( this.type ) + ':' + this.Size()
		}
		{	const _ = this.RawJSON()
			_ && ( $[ 'DATA' ] = _ )
		}
		{	const _ = AtomsJSON( this.children )
			_.length && ( $[ 'CHILDREN' ] = _ )
		}
		return $
	}

	RawJSON() {

		switch ( this.type ) {

		case Q_stsd	:
			return {
				'Version + Flags'							: this.Hex( 0, 4 )
			,	'Number of entries( Sample description )'	: this.Size4( 4 )
			}

		case Q_ftyp	:
		case Q_styp	:
			return {
				'Major brand'		: TypeString( this.Int4( 0 ) )
			,	'Minor version'		: this.Int4( 4 )
			,	'compatible_brands'	: Array.from(
					{ length: ( this.bytes.length - 8 ) / 4 }
				,	( $, _ ) => TypeString( this.Int4( 8 + _ * 4 ) )
				)
			}

		case Q_pssh	:
			{	const	vf = this.Size4( 0 )
				const	$ = {
					'Version + Flags'	: this.Hex( 0, 4 )
				,	SystemID			: this.Hex( 4, 16 )
				}
				let		pos = 20
				if ( vf >> 24 ) {
					const size = this.Size4( pos )
					$.KIDs = Array.from(
						{ length: size }
					,	( $, _ ) => this.Hex( pos + 4 + _ * 16, 16 )
					)
					pos += 4 + size * 16
				}
				const	size = this.Size4( pos );		pos += 4
				$.Data	= this.HexLF16( pos, size );	pos += size
				$.Extra	= this.Hex( pos, this.bytes.length - pos )
				return $
			}

		case Q_trex	:
			return {
				'Version + Flags'				: this.Hex( 0, 4 )
			,	TrackId							: this.Size4(  4 )
			,	DefaultSampleDescriptionIndex	: this.Size4(  8 )
			,	DefaultSampleDuration			: this.Size4( 12 )
			,	DefaultSampleSize				: this.Size4( 16 )
			,	DefaultSampleFlags				: this.Size4( 20 )
			//	<DefaultSampleFlags IsLeading="0" SampleDependsOn="0" SampleIsDependedOn="0" SampleHasRedundancy="0" SamplePadding="0" SampleSync="1" SampleDegradationPriority="0"/>
			}

		case Q_senc	:
			{	const	vf = this.Size4( 0 )
				const	$ = { 'Version + Flags'	: this.Hex( 0, 4 ) }
				let		pos = 4
				if ( vf & 1 ) {
					$[ 'AlgorithmId(3) + PerSampleIvthis.Size(1)'	]	= this.Size4( 4 )
					$.Kid												= this.Hex( 8, 16 )
					pos += 20
				}
				const	sampleCount = this.Size4( pos )
				pos += 4
				$.Entries = Array.from(
					{ length: sampleCount }
				,	() => {
						const $ = {}
						if ( IV_Size ) {
							$.InitializationVector = this.Hex( pos, IV_Size )
							pos += IV_Size
						}
						if ( vf & 2 ) {
							const subSampleCount = this.Size2( pos )
							$.subSamples = Array.from(
								{ length: subSampleCount }
							,	( $, _ ) => (
									{	nClear: this.Size2( pos + _ * 6 + 2 )
									,	nCrypt: this.Size4( pos + _ * 6 + 4 )
									}
								)
							)
							pos += 2 + subSampleCount * 6
						}
						return $
					}
				)
				return $
			}

		case Q_tkhd	:
			{	const	version = this.Size4( 0 ) >> 24
				const	$ = { 'Version + Flags'	: this.Hex( 0, 4 ) }
				let pos
				if ( version ) {
					$.CreationTime		= TimeString_MP4TimeStamp( this.Size8(  4 ) )
					$.ModificationTime	= TimeString_MP4TimeStamp( this.Size8( 12 ) )
					pos = 20
				} else {
					$.CreationTime		= TimeString_MP4TimeStamp( this.Size4(  4 ) )
					$.ModificationTime	= TimeString_MP4TimeStamp( this.Size4(  8 ) )
					pos = 12
				}
				$.TrackId			= this.Size4( pos )
				pos += 4
				pos += 4	//	Reserved1
				if ( version ) {
					$.Duration 		= this.Size8( pos )
					pos += 8
				} else {
					$.Duration 		= this.Size4( pos )
					pos += 4
				}
				pos += 8	//	Reserved2
				$.Layer				= this.Size2( pos )
				pos += 2
				$.AlternateGroup	= this.Size2( pos )
				pos += 2
				$.Volume			= this.Size2( pos + 4 )
				pos += 2
				pos += 2	//	Reserved3
				$.Matrix			= Array.from(
					{ length: 9 }
				,	( $, _ ) => this.Size4( pos + _ * 4 )
				)
				pos += 4 * 9
				$.Width				= this.Size4( pos ) / 0x10000
				pos += 4
				$.Height			= this.Size4( pos ) / 0x10000
				pos += 4
				return $
			}
			break

		case Q_tfhd	:
			{	const	vf = this.Size4( 0 )
				const	$ = { 'Version + Flags'	: this.Hex( 0, 4 ) }
				$.TrackId = this.Size4( 4 )
				let		pos = 8
				vf & 0x00001 && ( $.BaseDataOffset			= this.Size8( pos )		, pos += 8 )	//	TFHD_FLAG_BASE_DATA_OFFSET_PRESENT
				vf & 0x00002 && ( $.SampleDescriptionIndex	= this.Size4( pos )		, pos += 4 )	//	TFHD_FLAG_SAMPLE_DESCRIPTION_INDEX_PRESENT
				vf & 0x00008 && ( $.DefaultSampleDuration	= this.Size4( pos )		, pos += 4 )	//	TFHD_FLAG_DEFAULT_SAMPLE_DURATION_PRESENT
				vf & 0x00010 && ( $.DefaultSampleSize		= this.Size4( pos )		, pos += 4 )	//	TFHD_FLAG_DEFAULT_SAMPLE_SIZE_PRESENT
				vf & 0x00020 && ( $.DefaultSampleFlags		= this.Hex( pos, 4 )	, pos += 4 )	//	TFHD_FLAG_DEFAULT_SAMPLE_FLAGS_PRESENT
			//	 SamplePadding="0" Sync="0" DegradationPriority="0" IsLeading="0" DependsOn="1" IsDependedOn="0" HasRedundancy="0"
																									//	TFHD_FLAG_DURATION_IS_EMPTY
																									//	TFHD_FLAG_DEFAULT_BASE_IS_MOOF
				return $
			}

		case Q_tfdt	:
			return {
				'Version + Flags'	: this.Hex( 0, 4 )
			,	BaseMediaDecodeTime	: this.Size4( 0 ) >> 24 ? this.Size8( 4 ) : this.Size4( 4 )
			}

		case Q_avc1	:
		case Q_encv	:
			{	const $ = {
					Reserved			: this.Hex( 0, 6 )
				,	DataReferenceIndex	: this.Size2(  6 )
				,	Predefined1			: this.Size2(  8 )
				,	Reserved2			: this.Size2( 10 )
				,	Predefined2			: this.Hex( 12, 12 )
				,	Width				: this.Size2( 24 )
				,	Height				: this.Size2( 26 )
				,	HorizResolution		: this.Size4( 28 )
				,	VertResolution		: this.Size4( 32 )
				,	Reserved3			: this.Size4( 36 )
				,	FrameCount			: this.Size2( 40 )
				}
				$.CompressorName	= String.fromCharCode( ...SubBytes( this.bytes, 43, this.bytes[ 42 ] ) )
				$.Depth				= this.Size2( 74 )
				$.Predefined3		= this.Size2( 76 )
				return $
			}

		case Q_avcC	:
			{	const $ = {
					ConfigurationVersion	: this.Size1( 0 )
				,	Profile					: this.Size1( 1 )
				,	ProfileCompatibility	: this.Size1( 2 )
				,	Level					: this.Size1( 3 )
				,	NaluLengthSize			: 1 + ( this.Size1( 4 ) & 3 )
				}

				const	nSeqParams = this.Size1( 5 ) & 0x1f
				let		spekePos = 6

				$.SPSs = Array.from(
					{ length: nSeqParams }
				,	() => {
						const paramLength = this.Size2( spekePos )
						spekePos += 2
						const _ = AvcSPS( SubBytes( this.bytes, spekePos, paramLength ) )
						spekePos += paramLength
						return _
					}
				)

				const	nPicParams = this.Size1( spekePos )
				spekePos += 1
				$.PPSs = Array.from(
					{ length: nPicParams }
				,	() => {
						const paramLength = this.Size2( spekePos )
						spekePos += 2
						const _ = AvcPPS( SubBytes( this.bytes, spekePos, paramLength ) )
						spekePos += paramLength
						return _
					}
				)

				return $
			}

		case Q_schm	:
			return {
				'Version + Flags'	: this.Hex( 0, 4 )
			,	SchemeType			: TypeString( this.Int4( 4 ) )
			,	SchemeVersion		: this.Hex( 8, this.bytes.length < 12 ? 2 : 4 )
			}

		case Q_tenc	:
			{	const	vf = this.Size4( 0 )
				const	$ = { 'Version + Flags'	: this.Hex( 0, 4 ) }

				const	nBlocks = this.Size2( 4 )
				if ( vf >> 24 ) {
					$.DefaultCryptByteBlock	= nBlocks >> 8
					$.DefaultSkipByteBlock	= nBlocks & 0xff
				}

				$.DefaultIsProtected		= this.Size1( 6 )

				IV_Size						= this.Size1( 7 )
				$.DefaultPerSampleIvSize	= IV_Size

				$.DefaultKID				= this.Hex( 8, 16 )

				IV_Size || ( $.DefaultConstantIV = this.Hex( 25, this.Size1( 24 ) ) )

				return $
			}

		case Q_frma:
			return TypeString( this.Int4( 0 ) )

		case Q_stts	:
			return {
				'Version + Flags'	: this.Hex( 0, 4 )
			,	Entries				: Array.from(
					{ length: this.Size4( 4 ) }
				,	( $, _ ) => (
						{	'Sample count'		: this.Size4(  8 + 8 * _ )
						,	'Sample duration'	: this.Size4( 12 + 8 * _ )
						}
					)
				)
			}

		case Q_stsc	:
			return {
				'Version + Flags'	: this.Hex( 0, 4 )
			,	Entries				: Array.from(
					{ length: this.Size4( 4 ) }
				,	( $, _ ) => (
						{	'First chunk'			: this.Size4(  8 + 12 * _ )
						,	'Sample per chunk'		: this.Size4( 12 + 12 * _ )
						,	'Sample description ID'	: this.Size4( 16 + 12 * _ )
						}
					)
				)
			}

		case Q_stsz	:
			return {
				'Version + Flags'	: this.Hex( 0, 4 )
			,	'Sample size'		: this.Size4( 4 )
			,	Entries				: Array.from(
					{ length: this.Size4( 8 ) }
				,	( $, _ ) => this.Size4( 12 + 4 * _ )
				)
			}

		case Q_stco	:
			return {
				'Version + Flags'	: this.Hex( 0, 4 )
			,	Entries				: Array.from(
					{ length: this.Size4( 4 ) }
				,	( $, _ ) => this.Size4( 8 + 4 * _ )
				)
			}

		case Q_saiz	:
			{	const	vf = this.Size4( 0 )
				const	$ = { 'Version + Flags': this.Hex( 0, 4 ) }

				let		pos = vf & 1
				?	(	$.AuxInfoType			= this.Hex( 4, 4 )
					,	$.AuxInfoTypeParameter	= this.Hex( 8, 4 )
					,	12
					)
				:	4

				$.DefaultSampleInfoSize = this.Size1( pos )
				pos += 1

				if ( ! $.DefaultSampleInfoSize ) $.Entries = Array.from(
					{ length: this.Size4( pos ) }
				,	( $, _ ) => this.Size1( pos + 4 + _ )
				)
				return $
			}

		case Q_saio	:
			{	const	vf = this.Size4( 0 )
				const	$ = { 'Version + Flags': this.Hex( 0, 4 ) }
				let		pos = 4
				if ( vf & 1 ) {
					$.AuxInfoType			= this.Hex( 4, 4 )
					$.AuxInfoTypeParameter	= this.Hex( 8, 4 )
					pos = 12
				}
				$.Entries = Array.from(
					{ length: this.Size4( pos ) }
				,	( $, _ ) => vf >> 24
					?	this.Size8( pos + 4 + _ * 8 )
					:	this.Size4( pos + 4 + _ * 4 )
				)
				return $
			}

		case Q_trun	:
	//	const UI32 TRUN_FLAG_DATA_OFFSET_PRESENT					= 0x0001
	//	const UI32 TRUN_FLAG_FIRST_SAMPLE_FLAGS_PRESENT				= 0x0004
	//	const UI32 TRUN_FLAG_SAMPLE_DURATION_PRESENT				= 0x0100
	//	const UI32 TRUN_FLAG_SAMPLE_SIZE_PRESENT					= 0x0200
	//	const UI32 TRUN_FLAG_SAMPLE_FLAGS_PRESENT					= 0x0400
	//	const UI32 TRUN_FLAG_SAMPLE_COMPOSITION_TIME_OFFSET_PRESENT	= 0x0800
			{	const	vf = this.Size4( 0 )
				const	$ = { 'Version + Flags': this.Hex( 0, 4 ) }
				let		pos = 8
				if ( vf &	1 ) {
					$[ 'DataOffset(must be moof size + 8)' ] = this.Size4( pos )
					pos += 4
				}
				if ( vf &	2 ) pos += 4
				if ( vf &	4 ) {
					$.FirstSampleFlags = this.Hex( pos, 4 )
					pos += 4
				}
				if ( vf &	8 ) pos += 4
				if ( vf & 0x10 ) pos += 4
				if ( vf & 0x20 ) pos += 4
				if ( vf & 0x40 ) pos += 4
				if ( vf & 0x80 ) pos += 4
				$.Entries = Array.from(
					{ length: this.Size4( 4 ) }
				,	() => {
						const $ = {}
						if ( vf & 0x0100 ) {
							$.sample_duration = this.Size4( pos )
							pos += 4
						}
						if ( vf & 0x0200 ) {
							$.sample_size = this.Size4( pos )
							pos += 4
						}
						if ( vf & 0x0400 ) {
							$.sample_flags = this.Hex( pos, 4 )
							pos += 4
						}
						if ( vf & 0x0800 ) {
							$.sample_composition_time_offset = this.Size4( pos )
							pos += 4
						}
						if ( vf & 0x1000 ) pos += 4
						if ( vf & 0x2000 ) pos += 4
						if ( vf & 0x4000 ) pos += 4
						if ( vf & 0x8000 ) pos += 4
						return $
					}
				)
				return $
			}

		case Q_mvhd	:
			{	const	vf = this.Size4( 0 )
				const	$ = { 'Version + Flags'	: this.Hex( 0, 4 ) }
				if ( vf >> 24 ) {
					$.CreationTime		= TimeString_MP4TimeStamp( this.Size8(  4 ) )
					$.ModificationTime	= TimeString_MP4TimeStamp( this.Size8( 12 ) )
					$.TimeScale			= this.Size4( 20 )
					$.Duration			= this.Size8( 24 )
				} else {
					$.CreationTime		= TimeString_MP4TimeStamp( this.Size4(  4 ) )
					$.ModificationTime	= TimeString_MP4TimeStamp( this.Size4(  8 ) )
					$.TimeScale			= this.Size4( 12 )
					$.Duration			= this.Size4( 16 )
				}
				let	pos = vf >> 24 ? 32 : 20
				$.Rate		= this.Size4( pos )
				pos += 4
				$.Volume	= this.Size2( pos )
				pos += 2
				pos += 2	//	Reserved1
				pos += 8	//	Reserved2
				$.Matrix = Array.from(
					{ length: 9 }
				,	( $, _ ) => this.Size4( pos + _ * 4 )
				)
				pos += 9 * 4
				$.Predefined = this.Hex( pos, 24 )
				pos += 24
				$.NextTrackID = this.Int4( pos )
				return $
			}
			break

	//	case Q_iods	:
	//		s += 'Version + Flags:' + this.Hex( 0, 4 )
	//		{	const vf = this.Size4( 0 )
	//		}

		case Q_mdhd	:
			return this.Size4( 0 ) >> 24
			?	{	'Version + Flags'	: this.Hex( 0, 4 )
				,	CreationTime		: TimeString_MP4TimeStamp( this.Size8(  4 ) )
				,	ModificationTime	: TimeString_MP4TimeStamp( this.Size8( 12 ) )
				,	TimeScale			: this.Size4( 20 )
				,	Duration			: this.Size8( 24 )
				,	Lang				: this.Hex( 32, 2 )
				}
			:	{	'Version + Flags'	: this.Hex( 0, 4 )
				,	CreationTime		: TimeString_MP4TimeStamp( this.Size4(  4 ) )
				,	ModificationTime	: TimeString_MP4TimeStamp( this.Size4(  8 ) )
				,	TimeScale			: this.Size4( 12 )
				,	Duration			: this.Size4( 16 )
				,	Lang				: this.Hex( 20, 2 )
				}

		case Q_mehd	:
			return {
				'Version + Flags'	: this.Hex( 0, 4 )
			,	Duration			: this.Size4( 0 ) >> 24 ? this.Size8( 4 ) : this.Size4( 4 )
			}

		case Q_elst	:
			{	const	vf = this.Size4( 0 )
				const	$ = {
					'Version + Flags'	: this.Hex( 0, 4 )
				,	MaxEntries			: ( this.bytes.length - 8 ) / ( vf >> 24 ? 20 : 12 )
				}
				$.Entries = Array.from(
					{ length: this.Size4( 4 ) }
				,	( $, _ ) => vf >> 24
					?	{	'Segment Duration'	: this.Size8( 8 + _ * 20 )
						,	'Media Time'		: this.Int8( 8 + _ * 20 + 8 )
						,	'Media Rate'		: this.Size2( 8 + _ * 20 + 16 )
						}
					:	{	'Segment Duration'	: this.Size4( 8 + _ * 12 )
						,	'Media Time'		: this.Int4( 8 + _ * 12 + 4 )
						,	'Media Rate'		: this.Size2( 8 + _ * 12 + 8 )
						
						}
				)
				return $
			}

		case Q_hdlr	:
			return {
				'Version + Flags'	: this.Hex( 0, 4 )
			,	HandlerType			: TypeString( this.Int4( 8 ) )
			,	Name				: String.fromCharCode( ...SubBytes( this.bytes, 24 ) )
			}

		case Q_vmhd	:
			return {
				'Version + Flags'	: this.Hex( 0, 4 )
			,	GraphicsMode		: this.Size2(  4 )
			,	R					: this.Size2(  6 )
			,	G					: this.Size2(  8 )
			,	B					: this.Size2( 10 )
			}

		case Q_sidx	:
			{	const	vf = this.Size4( 0 )
				const	$ = {
					'Version + Flags'	: this.Hex( 0, 4 )
				,	RefernceID			: this.Size4( 4 )
				,	TimeScale			: this.Size4( 8 )
				}
				let pos = 12
				if ( vf >> 24 ) {
					$.EarliestPresentationTime	= this.Size8( pos );	pos += 8
					$.FirstOffset				= this.Size8( pos );	pos += 8
				} else {
					$.EarliestPresentationTime	= this.Size4( pos );	pos += 4
					$.FirstOffset				= this.Size4( pos );	pos += 4
				}
				pos += 2
				const referenceCount			= this.Size2( $, pos );	pos += 2
				$.References = Array.from(
					{ length: this.Size2( pos ) }
				,	( $, _ ) (
						{	'ReferenceType + ReferencedSize'	: this.Size4( pos +  2 + _ * 12 )
						,	SubsegmentDuration					: this.Size4( pos +  6 + _ * 12 )
						,	Sap									: this.Size4( pos + 10 + _ * 12 )
						}
					)
				)
				return $
			}

		case Q_data	:
			return String.fromCharCode( ...SubBytes( this.bytes, 8 ) )
//	<ToolBox Size="37" Type="A9746F6F" Specification="apple" Container="ilst data" value="Lavf58.29.100" >

		case Q_tfra	:
			{	const	vf = this.Size4( 0 )
				const	$ = {
					'Version + Flags'	: this.Hex( 0, 4 )
				,	TrackID				: this.Size4( 4 )
				}
				const	fields = this.Size4( 8 )
				let		pos = 16
				$.Entries = Array.from(
					{ length: this.Size4( 12 ) }
				,	() => {
						const $ = {}
						if ( vf >> 24 ) {
							$.Time			= this.Size8( pos );	pos += 8
							$.MoofOffset	= this.Size8( pos );	pos += 8
						} else {
							$.Time			= this.Size4( pos );	pos += 4
							$.MoofOffset	= this.Size4( pos );	pos += 4
						}
						switch ( ( fields >> 4 ) & 3 ) {
						case 0:	 $.TrafNumber	= this.Size1( pos ); pos += 1; break
						case 1:	 $.TrafNumber	= this.Size2( pos ); pos += 2; break
						case 2:	 $.TrafNumber	= this.Size3( pos ); pos += 3; break
						case 3:	 $.TrafNumber	= this.Size4( pos ); pos += 4; break
						}
						switch ( ( fields >> 2 ) & 3 ) {
						case 0:	 $.TrunNumber	= this.Size1( pos ); pos += 1; break
						case 1:	 $.TrunNumber	= this.Size2( pos ); pos += 2; break
						case 2:	 $.TrunNumber	= this.Size3( pos ); pos += 3; break
						case 3:	 $.TrunNumber	= this.Size4( pos ); pos += 4; break
						}
						switch ( fields & 3 ) {
						case 0:	 $.SampleNumber	= this.Size1( pos ); pos += 1; break
						case 1:	 $.SampleNumber	= this.Size2( pos ); pos += 2; break
						case 2:	 $.SampleNumber	= this.Size3( pos ); pos += 3; break
						case 3:	 $.SampleNumber	= this.Size4( pos ); pos += 4; break
						}
						return $
					}
				)
				return $
			}

		case Q_mfro	:
			return this.Size4( 4 )

		case Q_free	:
			return String.fromCharCode( ...this.bytes )

		case Q_mfhd	:
			return {
				'Version + Flags'	: this.Hex( 0, 4 )
			,	SequenceNumber		: this.Size4( 4 )
			}

		case Q_colr	:
			return EncodeHex( this.bytes )
		//	6e636c7800010001000100
		//	colour_type="nclx"
		//	colour_primaries="1" transfer_characteristics="1" matrix_coefficients="1" full_range_flag="0"

		case Q_pasp	:
			return EncodeHex( this.bytes )
		//	0000000100000001
		//	hSpacing="1" vSpacing="1"

		case Q_btrt	:
			return EncodeHex( this.bytes )
		//	000000000035fc260035fc26
		//	avgBitRate="3537958" maxBitRate="3537958"

		case Q_mp4a	:
			return EncodeHex( this.bytes )
		//	000000000000000100000000000000000002001000000000bb800000
		//	DataReferenceIndex="1" SampleRate="48000" Channels="2" BitsPerSample="16"

		case Q_esds	:
			return EncodeHex( this.bytes )
		//	0000000003190000000411401500000000016cd600016cd605021190060102
//						 <ES_Descriptor ES_ID="es0" >
//						  <decConfigDescr>
//						   <DecoderConfigDescriptor objectTypeIndication="64" streamType="5" maxBitrate="93398" avgBitrate="93398" >	//	93398 = 0x16cd6
//							<decSpecificInfo>
//							 <DecoderSpecificInfo type="auto" src="data:application/octet-string,%11%90" />
//							</decSpecificInfo>
//						   </DecoderConfigDescriptor>
//						  </decConfigDescr>
//						  <slConfigDescr>
//						   <SLConfigDescriptor >
//							<predefined value="2" />
//							<custom >
//							</custom>
//						   </SLConfigDescriptor>
//						  </slConfigDescr>
//						 </ES_Descriptor>

//		case Q_mdat	:
//			s += EncodeHexLF16( $, 0, $->$.size() )
//			break
//
//		default		:
//			s += EncodeHexLF16( $, 0, $->$.size() )
//			break
		}
	}
}

export const
Atoms = bytes => {
	const $ = []
	while ( bytes.length ) {
		const
		_			= MakeDataView( bytes )		

		const
		type		= _.getUint32( 4 )

		let	
		size		= _.getUint32()
		const
		headerSize	= size == 1
		?	(	size = _.getUint64( 8 )
			,	16
			)
		:	(	size = size
			,	8
			)

		const
		DataSize	= () => {
			switch ( type ) {
			case Q_moov	:
			case Q_trak	:
			case Q_edts	:
			case Q_mdia	:
			case Q_minf	:
			case Q_dinf	:
			case Q_stbl	:
			case Q_sinf	:
			case Q_schi	:
			case Q_mvex	:
			case Q_moof	:
			case Q_traf	:
			case Q_mfra	:
			case Q_udta	:
			case Q_ilst	:
			case Q_too	:
				return  0
			case Q_mp4a	:
			case Q_enca	:
				return 28
			case Q_meta	:
				return  4
			case Q_dref	:
			case Q_stsd	:
				return  8
			case Q_avc1	:
			case Q_encv	:
				return 78
			default		:
				return size - headerSize
			}
		}

		const
		dataSize	= DataSize()

		const
		offset		= headerSize + dataSize

		$.push(
			new Atom(
				type
			,	SubBytes( bytes, headerSize, dataSize )
			,	Atoms( SubBytes( bytes, offset, size - offset ) )
			)
		)

		bytes		= SubBytes( bytes, size )
	}
	return $
}

export const
WriteAtoms = ( atoms, bytes ) => {
	var	offset = 0
	atoms.forEach( atom => offset += atom.Write( SubBytes( bytes, offset ) ) )
	return offset
}

export const
JSON2Atom = json => {

	const	type = QUARTET( json.META )
	const	children = JSON2Atoms( json.CHILDREN ?? [] )

	let		data

	switch ( type ) {
	case Q_moov	:
	case Q_trak	:
	case Q_edts	:
	case Q_mdia	:
	case Q_minf	:
	case Q_dinf	:
	case Q_stbl	:
	case Q_sinf	:
	case Q_schi	:
	case Q_mvex	:
	case Q_moof	:
	case Q_traf	:
	case Q_mfra	:
	case Q_udta	:
	case Q_ilst	:
	case Q_too	:
		break
	case Q_mfhd	:
		data = new Uint8Array( 8 )
		{	const dv = MakeDataView( data )
			dv.setUint32(  0, parseInt( json.DATA[ "Version + Flags" ], 16 ) )
			dv.setUint32(  4, json.DATA.SequenceNumber )
		}
		break
	case Q_tfhd	:
		//	TODO	Look into flags
		data = new Uint8Array( 20 )
		{	const dv = MakeDataView( data )
			dv.setUint32(  0, parseInt( json.DATA[ "Version + Flags" ], 16 ) )
			dv.setUint32(  4, json.DATA.TrackId )
			dv.setUint32(  8, json.DATA.SampleDescriptionIndex )
			dv.setUint32( 12, json.DATA.DefaultSampleDuration )
			dv.setUint32( 16, parseInt( json.DATA.DefaultSampleFlags, 16 ) )
		}
		break
	case Q_tfdt	:
		//	TODO	Look into flags
		data = new Uint8Array( 12 )
		{	const dv = MakeDataView( data )
			dv.setUint32(  0, parseInt( json.DATA[ "Version + Flags" ], 16 ) )
			dv.setBigUint64( 4, BigInt( json.DATA.BaseMediaDecodeTime ) )
		}
		break
	case Q_trun	:
		//	TODO	Look into flags
		{	const entries = json.DATA.Entries
			data = new Uint8Array( 16 + entries.length * 4 )
			const dv = MakeDataView( data )
			dv.setUint32(  0, parseInt( json.DATA[ "Version + Flags" ], 16 ) )
			dv.setUint32(  4, entries.length )
			dv.setUint32(  8, json.DATA[ "DataOffset(must be moof size + 8)" ] )
			dv.setUint32( 12, parseInt( json.DATA.FirstSampleFlags, 16 ) )
			entries.forEach( ( $, _ ) => dv.setUint32( 16 + 4 * _, $.sample_size ) )
		}
		break
	}
	return new Atom( type, data ? data : new Uint8Array(), children )
}

export const
JSON2Atoms = json => json.map( JSON2Atom )

