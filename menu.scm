
; to be represented with something like
; 
;      x x x x x x x x x
;    xx                 x
;    x xx x x x x x x x
;    x               x
;    x               x
;    x               x
;    x               x
;    x               x
;  xxx               x
; x  x               x
;  xx x x x x x x x x
; 
; in the upper right corner of all games, along with other game-specific buttons

(define-module lib-menu

	(export 
		show-menu		;; menu choices -> action
	)

	; (show-menu menu choices) -> action
	;
	; action = #(quit)            -> exit menu and program, hangup or clicked quit
	;          #(update choices') -> possibly changed some preferenes
	;			  #(cancel)          -> exit menu ignoring changes

	; menu is 
	;	| #(menu <name> <description> <content>)
	;		name and description are strings (name show on list, description on top)
	;	| #(choose <name> <description> id <options>)
	;		where id is the key to set in opts
	;		and options is a list of option-nodes (only)
	;			name is the string to show in select meneu
	;			value is what to store to opts under the key id
	;  | #(back) - down to previous level in menu, returning changes
	;  | #(option name description value) - down to previous level in menu, returning changes
	;  | #(quit) - 
	;
	; opts is a ff of symbol -> value, so that it is easy to use (no hierarchy there)

	; drawing the menus:
	;	entry is always to a menu node. show the <description> on top
	;  then show a list of options and choose by click position, like olgame start menu
	; drawing a back button is just a <- back
	; drawing a menu could be colour-coded along with some marker
	; drawing a choose-node could show the <name> and the name of currently selected value (with a different color)
	; quit-node should be colour-coded separately

	; also handles scrolling later (add up- and down-buttons to side)
	(define (pick-thing things opts)
		; - draw representations of things 
		;	-> ff of y -> menu-node
		; - wait for a click in a suitable position
		; - return the thing from things
		)

	(define (choose-menu name desc id options opts)
		(show-label name desc)
		(tuple-case (pick-thing options opts)
			((option name desc value) value)
			(else is bad
				(error "choose menu had a non-option: " bad))))

	(define (show-menu menu opts)
		(tuple-case menu
			((menu name desc stuff)
				(show-label name desc)
				(tuple-case (pick-thing stuff opts)
					((quit) (tuple 'quit)) ; quit from this menu
					((back) (tuple 'save opts)) ; return with changes
					((choose name desc id options)
						(show-menu menu
							(put opts id (choose-menu name desc id options opts))))
					(else
						(error "show-menu: wtf is a " menu))))
			(else
				(error "show-menu: this is no menu: " menu))))

)
