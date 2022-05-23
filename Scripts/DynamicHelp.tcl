#*
#*
#*  System        : 
#*  Module        : 
#*  Object Name   : $RCSfile$
#*  Revision      : $Revision$
#*  Date          : $Date$
#*  Author        : $Author$
#*  Created By    : Robert Heller
#*  Created       : Thu May 16 09:05:46 2013
#*  Last Modified : <220523.1219>
#*
#*  Description	
#*
#*  Notes
#*
#*  History
#*	
#*	
#*
#*
#*     Generic Project
#*     Copyright (C) 2013  Robert Heller D/B/A Deepwoods Software
#* 			51 Locke Hill Road
#* 			Wendell, MA 01379-9728
#* 
#*     This program is free software; you can redistribute it and/or modify
#*     it under the terms of the GNU General Public License as published by
#*     the Free Software Foundation; either version 2 of the License, or
#*     (at your option) any later version.
#* 
#*     This program is distributed in the hope that it will be useful,
#*     but WITHOUT ANY WARRANTY; without even the implied warranty of
#*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#*     GNU General Public License for more details.
#* 
#*     You should have received a copy of the GNU General Public License
#*     along with this program; if not, write to the Free Software
#*     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#* 
#*  
#* 

## @addtogroup TclCommon
# @{

package require Tk
package require tile
package require snit

snit::macro DynamicHelp_include {type} {
    ## Macro to add Dynamic Help
    # Add options and support for Dynamic Help to widgets.
    
    option -helptext -default "" -configuremethod _configurehelp
    option -helpvar  -default "" -configuremethod _configurehelp
    option -helptype -default $type \
          -type {snit::enum -values {balloon variable}} \
          -configuremethod _configurehelp
    method _configurehelp {option value} {
        set options($option) $value
        set htype $options(-helptype)
        switch $htype {
            balloon {
                DynamicHelp register $win balloon "$options(-helptext)"
            }
            variable {
                DynamicHelp register $win variable $options(-helpvar) \
                      "$options(-helptext)"
            }
        }
    }
}

snit::type DynamicHelp {
    ## Implements BWidget style Dynamic Help: balloon help and status 
    # variable help displays
    
    pragma -hastypeinfo    no
    pragma -hastypedestroy no
    pragma -hasinstances   no
    
    typevariable _registered -array {}
    ## @privatesection Array of registered items with Dynamic Help.
    typevariable _canvases -array {}
    ## Array of Dynamic Help canvases.
    typevariable _top     ".help_shell"
    ## Help shell widget.
    typevariable _id      ""
    ## Help id.
    typevariable _delay   600
    ## Delay period.
    typevariable _current_balloon ""
    ## Current balloon.
    typevariable _current_variable ""
    ## Current variable.
    typevariable _saved
    ## 
    typevariable _options -array {
        -delay 600
        -state "normal"
    }
    ## Options array
    
    typeconstructor {
        ## Type constructor: one time initaization.
        bind BwHelpBalloon <Enter>   [myproc _motion_balloon enter  %W %X %Y]
        bind BwHelpBalloon <Motion>  [myproc _motion_balloon motion %W %X %Y]
        bind BwHelpBalloon <Leave>   [myproc _motion_balloon leave  %W %X %Y]
        bind BwHelpBalloon <Button>  [myproc _motion_balloon button %W %X %Y]
        bind BwHelpBalloon <Destroy> [myproc _unset_help %W]

        bind BwHelpVariable <Enter>   [myproc _motion_info %W]
        bind BwHelpVariable <Motion>  [myproc _motion_info %W]
        bind BwHelpVariable <Leave>   [myproc _leave_info  %W]
        bind BwHelpVariable <Destroy> [myproc _unset_help  %W]

        bind BwHelpMenu <<MenuSelect>> [myproc _menu_info select %W]
        bind BwHelpMenu <Unmap>        [myproc _menu_info unmap  %W]
        bind BwHelpMenu <Destroy>      [myproc _unset_help %W]
        snit::integer delaytype -min 100 -max 2000
        snit::enum    states    -values {normal disabled}
    }

    typemethod configure {args} {
        ## @publicsection Set configuration options.
        # @params ... Option value list: option value ...
        # @arg -state configure new state value.
        # @arg -delay configure new delay value.
        
        foreach {option value} $args {
            switch $option {
                -state {
                    states validate $value
                    set _options(-state) $value
                }
                -delay {
                    delaytype validate $value
                    set _options(-delay) $value
                    set _delay $value
                }
                default {
                    error "No such option: $option"
                }
            }
        }
    }
    typemethod register { path _type args } {
        ## Register a new widget with help.
        # @param path Widget path.
        # @param _type Type of help (variable or balloon)
        # @param ... Additional args (depends on type).
        
        set len [llength $args]
        if {$_type == "balloon"  && $len > 1} { set _type canvasBalloon  }
        if {$_type == "variable" && $len > 2} { set _type canvasVariable }
        if { ![winfo exists $path] } {
            _unset_help $path
            return 0
        }
        switch $_type {
            balloon {
                set text [lindex $args 0]
                if {$text == ""} {
                    if {[info exists _registered($path,balloon)]} {
                        unset _registered($path,balloon)
                    }
                    return 0
                }
                
                _add_balloon $path $text
            }
            canvasBalloon {
                set tagOrItem  [lindex $args 0]
                set text       [lindex $args 1]
                if {$text == ""} {
                    if {[info exists _registered($path,$tagOrItem,balloon)]} {
                        unset _registered($path,$tagOrItem,balloon)
                    }
                    return 0
                }
                
                _add_canvas_balloon $path $text $tagOrItem
            }
            
            variable {
                set var  [lindex $args 0]
                set text [lindex $args 1]
                if {$text == "" || $var == ""} {
                    if {[info exists _registered($path,variable)]} {
                        unset _registered($path,variable)
                    }
                    return 0
                }
                
                _add_variable $path $text $var
            }
            
            canvasVariable {
                set tagOrItem  [lindex $args 0]
                set var        [lindex $args 1]
                set text       [lindex $args 2]
                if {$text == "" || $var == ""} {
                    if {[info exists _registered($path,$tagOrItem,variable)]} {
                        unset _registered($path,$tagOrItem,variable)
                    }
                    return 0
                }
                
                _add_canvas_variable $path $text $var $tagOrItem
            }
            
            menu {
                set var [lindex $args 0]
                if {$var == ""} {
                    set cpath [_clonename $path]
                    if {[winfo exists $cpath]} { set path $cpath }
                    if {[info exists _registered($path)]} {
                        unset _registered($path)
                    }
                    return 0
                }
                
                _add_menu $path $var
            }

            menuentry {
                set cpath [_clonename $path]
                if { [winfo exists $cpath] } { set path $cpath }
                if {![info exists _registered($path)]} { return 0 }
                
                set text  [lindex $args 1]
                set index [lindex $args 0]
                if {$text == "" || $index == ""} {
                    set idx [lsearch $_registed($path) [list $index *]]
                    set _registered($path) [lreplace $_registered($path) $idx $idx]
                    return 0
                }
                
                _add_menuentry $path $text $index
            }
            
            default {
                _unset_help $path
                return 0
            }
        }

        return 1
    }
    typemethod add { path args } {
        ## Add a path to the list of paths with Dynamic Help.
        # @param path The widget path.
        # @param ... Options:
        # @arg -type The type of Dynamic Help.
        # @arg -text The help text.
        # @arg -item The menu item.
        # @arg -index The item index.
        # @arg -command The command.
        # @arg -variable The variable.
        
        array set data {
            -type     balloon
            -text     ""
            -item     ""
            -index    -1
            -command  ""
            -variable ""
        }
        if {[winfo exists $path] && [winfo class $path] == "Menu"} {
            set data(-type) menu
        }
        array set data $args

        set item $path

        switch -- $data(-type) {
            "balloon" {
                if {$data(-item) != ""} {
                    _add_canvas_balloon $path $data(-text) $data(-item)
                    set item $path,$data(-item)
                } else {
                    _add_balloon $path $data(-text)
                }

                if {$data(-variable) != ""} {
                    set _registered($item,balloonVar) $data(-variable)
                }
            }

            "variable" {
                set var $data(-variable)
                if {$data(-item) != ""} {
                    _add_canvas_variable $path $data(-text) $var $data(-item)
                    set item $path,$data(-item)
                } else {
                    _add_variable $path $data(-text) $var
                }
            }

            "menu" {
                if {$data(-index) != -1} {
                    set cpath [_clonename $path]
                    if { [winfo exists $cpath] } { set path $cpath }
                    if {![info exists _registered($path)]} { return 0 }
                    _add_menuentry $path $data(-text) $data(-index)
                    set item $path,$data(-index)
                } else {
                    _add_menu $path $data(-variable)
                }
            }

            default {
                return 0
            }
        }

        if {$data(-command) != ""} {set _registered($item,command) $data(-command)}
        
        return 1
    }
    
    typemethod delete { path } {
        ## Delete a path from Dynamic Help.
        # @param path The widget path.
        
        _unset_help $path
    }
    
    proc _add_bind_tag { path tag } {
        ## @privatesection Add a bind tag.
        # @param path The widget path.
        # @param tag The bind tag to add.
        
        set evt [bindtags $path]
        set idx [lsearch $evt $tag]
        set evt [lreplace $evt $idx $idx]
        lappend evt $tag
        bindtags $path $evt
    }
    proc _add_balloon { path text } {
        ## Add a Dynamic Help balloon.
        # @param path The widget path.
        # @param text The help text.
        
        set _registered($path,balloon) $text
        _add_bind_tag $path BwHelpBalloon
    }
    proc _add_canvas_balloon { path text tagOrItem } {
        ## Add a Dynamic Help canvas balloon.
        # @param path The widget path.
        # @param text The help text.
        # @param tagOrItem The tag or item.
        
        set _registered($path,$tagOrItem,balloon) $text

        if {![info exists _canvases($path,balloon)]} {
            ## This canvas doesn't have the bindings yet.

            _add_bind_tag $path BwHelpBalloon

            $path bind BwHelpBalloon <Enter> \
                  [list DynamicHelp::_motion_balloon enter  %W %X %Y 1]
            $path bind BwHelpBalloon <Motion> \
                  [list DynamicHelp::_motion_balloon motion %W %X %Y 1]
            $path bind BwHelpBalloon <Leave> \
                  [list DynamicHelp::_motion_balloon leave  %W %X %Y 1]
            $path bind BwHelpBalloon <Button> \
                  [list DynamicHelp::_motion_balloon button %W %X %Y 1]

            set _canvases($path,balloon) 1
        }

        $path addtag BwHelpBalloon withtag $tagOrItem
    }

    proc _add_variable { path text varName } {
        ## Add a variable Dynamic Help.
        # @param path The widget path.
        # @param text The help text.
        # @param varName The variable to get the help text.
        
        set _registered($path,variable) [list $varName $text]
        _add_bind_tag $path BwHelpVariable
    }
    proc _add_canvas_variable { path text varName tagOrItem } {
        ## Add a canvas variable Dynamic Help.
        # @param path The widget path.
        # @param text The help text.
        # @param varName The variable to get the help text.
        # @param tagOrItem The tag or item.
        
        set _registered($path,$tagOrItem,variable) [list $varName $text]

        if {![info exists _canvases($path,variable)]} {
            ## This canvas doesn't have the bindings yet.

            _add_bind_tag $path BwHelpVariable

            $path bind BwHelpVariable <Enter> \
                  [list DynamicHelp::_motion_info %W 1]
            $path bind BwHelpVariable <Motion> \
                  [list DynamicHelp::_motion_info %W 1]
            $path bind BwHelpVariable <Leave> \
                  [list DynamicHelp::_leave_info  %W 1]

            set _canvases($path,variable) 1
        }

        $path addtag BwHelpVariable withtag $tagOrItem
    }
    proc _clonename { menu } {
        ## Clone a menu?
        # @param menu A menu
        # @returns a widget path.
        
        set path     ""
        set menupath ""
        set found    0
        foreach widget [lrange [split $menu "."] 1 end] {
            if { $found || [winfo class "$path.$widget"] == "Menu" } {
                set found 1
                append menupath "#" $widget
                append path "." $menupath
            } else {
                append menupath "#" $widget
                append path "." $widget
            }
        }
        return $path
    }
    proc _add_menu { path varName } {
        ## Add a menu with Dynamic Help
        # @param path The widget path.
        # @param varName The variable to get the help text.
        
        set cpath [_clonename $path]
        if { [winfo exists $cpath] } { set path $cpath }

        set _registered($path) [list $varName]
        _add_bind_tag $path BwHelpMenu
    }


    proc _add_menuentry { path text index } {
        ## Add menu entry to Dynamic Help.
        # @param path The widget path.
        # @param text The help text.
        # @param index The menu index.
        
        set idx  [lsearch $_registered($path) [list $index *]]
        set list [list $index $text]
        if { $idx == -1 } {
            lappend _registered($path) $list
        } else {
            set _registered($path) \
                  [lreplace $_registered($path) $idx $idx $list]
        }
    }


    # ----------------------------------------------------------------------------
    #  Command DynamicHelp::_motion_balloon
    # ----------------------------------------------------------------------------
    proc _motion_balloon { type path x y {isCanvasItem 0} } {
        ## Balloon motion binding
        # @param type One of enter, motion, leave, or button.
        # @param path The widget path.
        # @param x The screen X coord.
        # @param y The screen Y coord.
        # @param isCanvasItem Canvas item flag.
        
        set w $path
        if {$isCanvasItem} { set path [_get_canvas_path $path balloon] }

        if { $_current_balloon != $path && $type == "enter" } {
            set _current_balloon $path
            set type "motion"
            destroy $_top
        }
        if { $_current_balloon == $path } {
            if { $_id != "" } {
                after cancel $_id
                set _id ""
            }
            if { $type == "motion" } {
                if { ![winfo exists $_top] } {
                    set cmd [list DynamicHelp::_show_help $path $w $x $y]
                    set _id [after $_delay $cmd]
                }
            } else {
                destroy $_top
                set _current_balloon ""
            }
        }
    }
    
    proc _global_setvar  { varName value } {
        ## Set a global variable.
        # @param varName The variable name.
        # @param value The value.
        
        return [uplevel \#0 [list set $varName $value]]
    }
    
    proc _global_getvar  { varName } {
        ## Get a global variable.
        # @param varName The variable name.
        
        return [uplevel \#0 [list set $varName]]
    }
    
    

    # ----------------------------------------------------------------------------
    #  Command DynamicHelp::_motion_info
    # ----------------------------------------------------------------------------
    proc _motion_info { path {isCanvasItem 0} } {
        ## Motion info binding.
        # @param path The widget path.
        # @param isCanvasItem Canvas item flag.
        
        if {$isCanvasItem} { set path [_get_canvas_path $path variable] }

        if { $_current_variable != $path
            && [info exists _registered($path,variable)] } {

            set varName [lindex $_registered($path,variable) 0]
            if {![info exists _saved]} { set _saved [_global_getvar $varName] }
            set string [lindex $_registered($path,variable) 1]
            if {[info exists _registered($path,command)]} {
                set string [eval $_registered($path,command)]
            }
            _global_setvar $varName $string
            set _current_variable $path
        }
    }


    # ----------------------------------------------------------------------------
    #  Command DynamicHelp::_leave_info
    # ----------------------------------------------------------------------------
    proc _leave_info { path {isCanvasItem 0} } {
        ## Leave info binding.
        # @param path The widget path.
        # @param isCanvasItem Canvas item flag.
        
        if {$isCanvasItem} { set path [_get_canvas_path $path variable] }

        if { [info exists _registered($path,variable)] } {
            set varName [lindex $_registered($path,variable) 0]
            _global_setvar $varName $_saved
        }
        unset _saved
        set _current_variable ""
    }


    # ----------------------------------------------------------------------------
    #  Command DynamicHelp::_menu_info
    #    Version of R1v1 restored, due to lack of [winfo ismapped] and <Unmap>
    #    under windows for menu.
    # ----------------------------------------------------------------------------
    proc _menu_info { event path } {
        ## Menu info binding.
        # @param event The event.
        # @param path The widget path.
        
        if { [info exists _registered($path)] } {
            set index   [$path index active]
            set varName [lindex $_registered($path) 0]
            if { ![string equal $index "none"] &&
                [set idx [lsearch $_registered($path) [list $index *]]] != -1 } {
                set string [lindex [lindex $_registered($path) $idx] 1]
                if {[info exists _registered($path,$index,command)]} {
                    set string [eval $_registered($path,$index,command)]
                }
                _global_setvar $varName $string
            } else {
                _global_setvar $varName ""
            }
        }
    }


    # ----------------------------------------------------------------------------
    #  Command DynamicHelp::_show_help
    # ----------------------------------------------------------------------------
    proc _show_help { path w x y } {
        ## Show Dynamic Help.
        # @param w The widget path.
        # @param x The screen X coordinate.
        # @param y The screen Y coordinate.
        
        if { $_options(-state) eq "disabled" } { return }

        if { [info exists _registered($path,balloon)] } {
            destroy  $_top

            set string $_registered($path,balloon)

            if {[info exists _registered($path,balloonVar)]} {
                upvar #0 $_registered($path,balloonVar) var
                if {[info exists var]} { set string $var }
            }

            if {[info exists _registered($path,command)]} {
                set string [eval $_registered($path,command)]
            }

            if {$string eq ""} { return }

            ttk::toplevel $_top -style HelpBallon -class HelpBallon \
                  -screen [winfo screen $w]
            
            wm withdraw $_top
            if {$::tk_version >= 8.4
                && [string equal [tk windowingsystem] "aqua"]} {
                ::tk::unsupported::MacWindowStyle style $_top help none
            } else {
                wm overrideredirect $_top 1
            }

            catch { wm attributes $_top -topmost 1 }

            ttk::label $_top.label -text $string \
                  -style HelpBallonLabel -class HelpBallonLabel


            pack $_top.label -side left
            update idletasks

            if {![winfo exists $_top]} {return}

            set  scrwidth  [winfo vrootwidth  .]
            set  scrheight [winfo vrootheight .]
            set  width     [winfo reqwidth  $_top]
            set  height    [winfo reqheight $_top]
            incr y 12
            incr x 8

            if { $x+$width > $scrwidth } {
                set x [expr {$scrwidth - $width}]
            }
            if { $y+$height > $scrheight } {
                set y [expr {$y - 12 - $height}]
            }

            wm geometry  $_top "+$x+$y"
            update idletasks

            if {![winfo exists $_top]} { return }
            wm deiconify $_top
            raise $_top
        }
    }

    # ----------------------------------------------------------------------------
    #  Command DynamicHelp::_unset_help
    # ----------------------------------------------------------------------------
    proc _unset_help { path } {
        ## Unset help.
        # @param path The Widget path.
        
        if {[info exists _registered($path)]} { unset _registered($path) }
        if {[winfo exists $path]} {
            set cpath [_clonename $path]
            if {[info exists _registered($cpath)]} { unset _registered($cpath) }
        }
        catch {array unset _canvases   $path,*}
        catch {array unset _registered $path,*}
        catch {destroy $_top}
    }

    # ----------------------------------------------------------------------------
    #  Command DynamicHelp::_get_canvas_path
    # ----------------------------------------------------------------------------
    proc _get_canvas_path { path type {item ""} } {
        ## Get the canvas path.
        # @param path The Widget path.
        # @param type The type.
        # @param item The item.
        
        if {$item == ""} { set item [$path find withtag current] }

        ## Check the tags related to this item for the one that
        ## represents our text.  If we have text specific to this
        ## item or for 'all' items, they override any other tags.
        eval [list lappend tags $item all] [$path itemcget $item -tags]
        foreach tag $tags {
            set check $path,$tag
            if {![info exists _registered($check,$type)]} { continue }
            return $check
        }
    }
    

}

ttk::style configure HelpBallon -background black -relief flat -borderwidth 1
ttk::style configure HelpBallonLabel -relief flat -bd 0 -highlightthickness 0 \
      -padx 1 -pady 1 -foreground black -background "#FFFFC0" \
      -font "helvetica 8" -justify left

## @}

package provide DynamicHelp 1.0
