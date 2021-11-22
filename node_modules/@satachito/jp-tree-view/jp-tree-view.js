class
JPTreeView extends HTMLElement {
	constructor( _, open = false, depth = 0 ) {
		super()
		this.style.display = 'inline-block'
	//	this.style.verticalAlign = 'top'
		const
		OnA = ( name, cb ) => {
			const $ = this.getAttribute( name )
			$ != null && cb( $ )
		}
		OnA( 'json'	, $ => _		= JSON.parse( $ ) )
		OnA( 'open'	, $ => open		= true )
		OnA( 'depth', $ => depth	= $ )
		setTimeout( () => this.props( _, open, depth, 0 ), 0 )
	}
	props( $, open, depth, cur_depth = 0 ) {
		this.textContent = ''
		switch ( $ ) {
		case void 0:
			this.classList.add( `jp-tree-view-undefined` )
			this.textContent = 'undefined'
			break
		case null:
			this.classList.add( `jp-tree-view-null` )
			this.textContent = 'null'
			break
		default:
			this.classList.add( `jp-tree-view-${ $.constructor.name }` )
			switch ( $.constructor ) {
			case Object:
			case Array:
				const triangle = document.createElement( 'a' )
				triangle.onclick = () => this.props( $, !open, depth, cur_depth )
				if ( open && ( depth == 0 || cur_depth < depth ) ) {
					triangle.textContent = '▼ '
					this.appendChild( document.createTextNode( Array.isArray( $ ) ? '[' : '{' ) )
					this.appendChild( triangle )
					this.appendChild( document.createElement( 'br' ) )
					Object.keys( $ ).forEach(
						key => {
							const keySpan = this.appendChild( document.createElement( 'span' ) )
							keySpan.textContent = key
							keySpan.classList.add( 'jp-tree-view-key' )
							this.appendChild( new JPTreeView( $[ key ], open, depth, cur_depth + 1 ) )
							this.appendChild( document.createElement( 'br' ) )
						}
					)
					this.appendChild( document.createTextNode( Array.isArray( $ ) ? ']' : '}' ) )
				} else {
					triangle.textContent = '▶ '
					this.appendChild( triangle )
					this.appendChild( document.createTextNode( `${ Array.isArray( $ ) ? '[...]' : '{...}' }(${ Object.keys( $ ).length })` ) )
				}
				break
			default:
				this.textContent = $
				break
			}
			break
		}
	}
}
customElements.define( 'jp-tree-view', JPTreeView )
