#* 
#* ------------------------------------------------------------------
#* Role PlayingDB V3.0 by Deepwoods Software
#* ------------------------------------------------------------------
#* RPGUtilities.tcl - Various widgets/widget adapters
#* Created by Robert Heller on Thu Sep  3 09:26:42 2009
#* ------------------------------------------------------------------
#* $Id$
#* ------------------------------------------------------------------
#* Contents:
#* ------------------------------------------------------------------
#*  
#*     Role Playing DB -- A database package that creates and maintains
#* 		       a database of RPG characters, monsters, treasures,
#* 		       spells, and playing environments.
#* 
#*     Copyright (C) 2009  Robert Heller D/B/A Deepwoods Software
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

## @addtogroup RPGSupport
# @{


#package require BWidget
package require Tk
package require tile
package require IconImage
package require snit
package require ParseXML
#package require BWLabelComboBox
#package require BWLabelSpinBox
#package require BWFileEntry
#package require LabelSelectColor
package require LabelFrames
package require pdf4tcl
package require Dialog
package require ButtonBox
package require ScrollableFrame

namespace eval RolePlayingDB3 {
    ## Contains all utility code specific to the RolePlayingDB3 system.
    
    snit::widgetadaptor LabeledDirTree {
        ## Labeled Directory Tree Widget
        #
        
        option  -auto -readonly yes -default both \
              -type {snit::enum -values {none both vertical horizontal}}
        option  -scrollbar -readonly yes -default both \
              -type {snit::enum -values {none both vertical horizontal}}
        option {-showextension showExtension ShowExtension} \
              -type snit::boolean -default yes
        option -directory -validatemethod validdirectory \
              -configuremethod newdirectory \
              -default {}
        option {-filepattern filePattern FilePattern} -default *
        option {-sortfunction sortFunction SortFunction} -default {} \
					-configuremethod newdirectory
        option {-opendirs openDirs OpenDirs} -default yes -type snit::boolean \
              -configuremethod newdirectory
        option {-nofiles  noFiles NoFiles} -default no -type snit::boolean \
              -configuremethod newdirectory
        typevariable openfold
        ## @privatesection Openfolder icon.
        method validdirectory {option value} {
            ## Validate a directory
            # @param name The option (-directory).
            # @param value The directory.
            # @throws type-error If the argument is not a directory. 

            #if {"$value" eq ""} {return};# Special non-value
            if {[file isdirectory "$value"] && [file readable "$value"]} {
                return $value
            } else {
                error "Expected a readable directory for $option, got $value"
            }
        }
        method newdirectory {option value} {
            ## Updates the directory option.
            # @param option The option (-directory).
            # @param value The directory.
            
            set options($option) "$value"
            $self redrawdirtree
        }
        typeconstructor {
            ## Type constructor.  Loads the openfold icon.
            set openfold [IconImage image openfold]
        }
        component scroll
        ## Scroll window.
        component tree
        ## treeview
        delegate option -label to hull as -text
        delegate method * to tree except {cget configure xview yview add insert 
            delete move reorder item parent}
        delegate option * to tree except {-xscrollcommand -yscrollcommand}
        method itemcget {node option} {
            ## @publicsection Gets an option from an item.
            # @param node Item to get an option from.
            # @param option The option to fetch.
            # @returns The option value.
            # @throws Unsupported-Option For unsupported options.
            
            switch -glob -- $option {
                -fullpath {return [$tree item $node -values]}
                -dirnode  {return [$tree parent   $node]}
                default {
                    error "Unsuported option $option, should be one of -fullpath or -dirnode"
                }
            }
        }
        proc dirtree {tree parent dir pattern showextension sortfunction opendirs nofiles} {
            ## @privatesection Load a directory tree.
            # @param tree treeview widget.
            # @param parent parent node.
            # @param dir directory.
            # @param pattern file pattern.
            # @param showextension show extension?
            # @param sortfunction sort function
            # @param opendirs Open directory subtrees?
            # @param nofiles Don't show files?
            
            #      puts stderr "*** [namespace which dirtree] $tree $parent $dir $pattern $showextension"
            set unsorteddirs [glob -nocomplain -type d [file join $dir *]]
            if {"$sortfunction" eq ""} {
                set sorteddirs [lsort -dictionary $unsorteddirs]
            } else {
                set sorteddirs [lsort -command $sortfunction $unsorteddirs]
            }
            foreach directory $sorteddirs {
                regsub -all {[[:space:]]} "[file rootname [file tail $directory]]" {_} nodename
                #puts stderr "*** [namespace which dirtree]: directory = $directory, nodename = $nodename"
                set thisnode [$tree insert "$parent" end  \
                              -values "$directory" \
                              -text "[file tail $directory]" \
                              -open $opendirs \
                              -image "$openfold" -tag directory]
                dirtree $tree $thisnode $directory $pattern $showextension $sortfunction $opendirs $nofiles
            }
            if {$nofiles} {return}
            set unsortedfiles [glob -nocomplain -type f [file join $dir $pattern]]
            if {"$sortfunction" eq ""} {
                set sortedfiles [lsort -dictionary $unsortedfiles]
            } else {
                set sortedfiles [lsort -command $sortfunction $unsortedfiles]
            }
            foreach file $sortedfiles {
                if {"[file tail $file]" eq "flag"} {continue}
                regsub -all {[[:space:]]} "[file rootname [file tail $file]]" {_} nodename
                #puts stderr "*** [namespace which dirtree]: file = $file, nodename = $nodename"
                if {$showextension} {
                    set text [file tail $file]
                } else {
                    set text [file rootname [file tail $file]]
                }
                $tree insert $parent end \
                      -values $file \
                      -text "$text" \
                      -open no \
                      -tag file
            }
        }
        method redrawdirtree {} {
            ## @publicsection Redraw directory tree
            
            if {"$options(-directory)" eq ""} {return}
            $tree delete [$tree children {}]
            dirtree $tree {} $options(-directory) $options(-filepattern) \
                  $options(-showextension) $options(-sortfunction) \
                  $options(-opendirs) $options(-nofiles)
        }
        method binditem {what sequence script} {
            ## Bind events to items
            # @param what What tag to bind to.
            # @param sequence Sequence to bind.
            # @param script Strict to bind.
            
            $tree tag bind $what $sequence [mymethod _bindHelper %x %y $script]
        }
        method _bindHelper {x y script} {
            ## @privatesection Bind helper (simulate BWidgets Tree widget)
            # @param x Mouse X
            # @param y Mouse y
            # @param script Script to run
            
            set item [$tree identify item $x $y]
            if {$item ne {}} {
                uplevel #0 "$script $item"
            }
        }
        constructor {args} {
            ## @publicsection Constructor.  Build the widget.
            # @param widgetpath The widget path.
            # @param ... Options:
            # @arg -auto Passed to the scrolling window.
            # @arg -scrollbar Passed to the scrolling window.
            # @arg -showextension Show the file's extension.
            # @arg -directory The root directory.
            # @arg -filepattern The file pattern to use.
            # @arg -sortfunction Sorting function.
            # @arg -opendirs Open dirs initially or not.
            # @arg -nofiles Controls whether to include files or not.
            # @arg -label Label text for the widget.
            # @par
            
            set options(-auto) [from args -auto]
            set options(-scrollbar) [from args -scrollbar]
            installhull using labelframe
            install scroll using ScrolledWindow $win.scroll \
                  -auto $options(-auto) \
                  -scrollbar $options(-scrollbar)
            pack $scroll -fill both -expand yes
            install tree using ttk::treeview [$scroll getframe].tree -show tree \
                  -selectmode browse
            $scroll setwidget $tree
            $self configurelist $args
        }
    }
    snit::widget ScrolledCanvas {
        ## Scrolled Canvas
        # Just a plain canvas, with scrollbars.
        # All of the canvas's methods are exposed except xview yview.
        
        component scroll
        component canvas
        delegate option * to canvas except {-xscrollcommand -yscrollcommand}
        delegate method * to canvas except {cget configure xview yview}
        option  -auto -readonly yes -default both \
              -type {snit::enum -values {none both vertical horizontal}}
        option  -scrollbar -readonly yes -default both \
              -type {snit::enum -values {none both vertical horizontal}}
        constructor {args} {
            ## Constructor -- build a scrolled canvas.
            # @param widgetpath The widget path
            # @param ... Options:
            # @arg -auto Passed to scroll window.
            # @arg -scrollbar Passed to scroll window.
            # @par All of the options (except -xscrollcommand -yscrollcommand)
            # that the canvas command takes.
            
            set auto [from args -auto]
            set scrollbar [from args -scrollbar]
            install scroll using ScrolledWindow $win.scroll \
                  -auto $auto -scrollbar $scrollbar
            pack $scroll -fill both -expand yes
            install canvas using canvas [$scroll getframe].canvas
            $scroll setwidget $canvas
            $self configurelist  $args
        }
        method bindcanvas {args} {
            ## Feed through bind to the canvas
            # @param ... See 3tk bind
            
            return [::bind $canvas {*}$args]
        }
    }
    snit::widget ScrolledList {
        ## Scrolled Listbox
        # many ttk::treeview methods passed through to the list, otherwise 
        # methods to simulate BWindget List are provided.
        
        component scroll
        component list
        delegate option * to list except {-xscrollcommand -yscrollcommand 
            -show -columns -displaycolumns}
        delegate method * to list except {children item insert add cget configure 
            xview yview}
        option  -auto -readonly yes -default both \
              -type {snit::enum -values {none both vertical horizontal}}
        option  -scrollbar -readonly yes -default both \
              -type {snit::enum -values {none both vertical horizontal}}
        method items {} {
            ## Return a list of item ids in the list 
            # @returns a list of item ids.
            return [$list children {}]
        }
        method itemcget {item option} {
            ## Get an item's property
            # @param item Item id.
            # @param option The option to fetch.
            # @returns the item property.
            
            return [$list item $item $option]
        }
        method insert {where args} {
            ## Insert an item at a specified location.
            # @param where Index to insert at (end == at the end).
            # @param ... Options: passed through to ttk::treview's insert 
            # method.
            
            $list insert {} $where {*}$args
        }
        
        constructor {args} {
            ## Constructor -- build a scrolled list box.
            # @param widgetpath 
            # @param ... Options:
            # @arg -auto Passed to scroll window.
            # @arg -scrollbar Passed to scroll window.
            # @par All of the options (except -xscrollcommand, -yscrollcommand,
            # -show, -columns, and -displaycolumns) that the ttk::treeview 
            # command takes.
            set auto [from args -auto]
            set scrollbar [from args -scrollbar]
            install scroll using ScrolledWindow $win.scroll \
                  -auto $auto -scrollbar $scrollbar
            pack $scroll -fill both -expand yes
            install list using ttk::treeview [$scroll getframe].list \
                  -selectmode [from args -selectmode none] \
                  -show {} -columns {descr attrList} -displaycolumns {descr}
            $scroll setwidget $list
            $self configurelist  $args
        }
    }
    snit::widget Graphic {
        ## Build a widget to fetch a graphcs file and display the image.
        
        component label
        component fileentry
        option -modifycmd -default {}
        option -text -default {} -cgetmethod gettext -configuremethod settext
        option {-basedirectory baseDirectory BaseDirectory} -default {}
        delegate option -label to fileentry
        delegate option -labelwidth to fileentry
        delegate option -initialdir to fileentry
        method gettext {option} {
            ## @privatesection Fetch the text from the fileentry.
            # @param option Always -text
            # @returns the value of the -text option of the fileentry
            return [$fileentry cget $option]
        }
        method settext {option value} {
            ## Set the text in the fileentry.
            # Side effect: the graphic is updated.
            # @param option Always -text
            # @param value New value of the fileentry's -text option.
            
            $fileentry configure $option $value
            $self updatepicture
        }
        method getimage {} {
            ## @publicsection Fetch the graphic image object.
            # @returns an image object
            return [$label cget -image]
        }
        constructor {args} {
            ## Build a widget to fetch and display an image file.
            # @param widgetpath The widget path
            # @param ... Options:
            # @arg -modifycmd Script to run when the widget is updated.
            # @arg -text Filename in the file entry.
            # @arg -basedirectory Base directory.
            # @arg -label Label for the fileentry.
            # @arg -labelwidth Label width for the fileentry.
            # @arg -initialdir Initial dir for the fileentry.
            
            install label using tk::label $win.label
            pack $label -fill both
            install fileentry using FileEntry $win.fileentry -filetypes { 
                {"BMP Files"        {.bmp}              }
                {"GIF Files"        {.gif .GIF}     GIFf}
                {"JPEG Files"       {.jpeg .jpg}    JPEG}
                {"PNG Files"        {.png}          PNGF}
                {"TIFF Files"       {.tiff .tif}    TIFF}
                {"XBM Files"        {.xbm}              }
                {"XPM Files"        {.xpm}              }
                {"Postscript Files" {.ps .eps}          } 
                {"All Files"        *                   } } \
                  -modifycmd [mymethod filemodified]
            pack $fileentry -fill x
            $fileentry bind <Return> [mymethod filemodified]
            $self configurelist  $args
        }
        method updatepicture {} {
            ## @privatesection Updata the picture when the filename changes.
            
            set fullpath [$fileentry get]
            #      puts stderr "*** $self updatepicture: fullpath (from FE) is $fullpath"
            #      puts stderr "*** $self updatepicture: options(-basedirectory) is $options(-basedirectory)"
            #      puts stderr "*** $self updatepicture: pathtype of $fullpath is [file pathtype $fullpath]"
            if {[file pathtype $fullpath] eq "relative"} {
                set fullpath [file join $options(-basedirectory) $fullpath]
            }
            #      puts stderr "*** $self updatepicture: fullpath (final) is $fullpath"
            if {[catch {image create photo -file $fullpath} newimage]} {
                #puts stderr "*** $self updatepicture: image create photo failed: $newimage"
                return
            }
            #      puts stderr "*** $self updatepicture: newimage is $newimage"
            set oldimg [$label cget -image]
            if {"$oldimg" ne ""} {image delete $oldimg}
            $label configure -image $newimage
        }
        method filemodified {} {
            ## Filename modified hook.
            $self updatepicture
            if {"$options(-modifycmd)" ne ""} {
                uplevel \#0 "$options(-modifycmd)"
            }
        }
    }
    snit::widgetadaptor LabeledScrolledText {
        ## Labeled Scrolled Text widget

        component scroll
        component text
        delegate option * to text except {-xscrollcommand -yscrollcommand}
        delegate method * to text except {cget configure xview yview}
        delegate option -label to hull as -text
        option  -auto -readonly yes -default both \
              -type {snit::enum -values {none both vertical horizontal}}
        option  -scrollbar -readonly yes -default both \
              -type {snit::enum -values {none both vertical horizontal}}
        option -text -cgetmethod gettext -configuremethod puttext
        method gettext {option} {
            ## @privatesection Fetch the text from the text widget
            # @param option Always -text
            return "[$text get 1.0 end-1c]"
        }
        method puttext {option value} {
            ## Set the text in the Text widget
            # @param option Always -text
            # @param value the new text.
            $text delete 1.0 end
            $text insert end "$value"
        }
        method bind {args} {
            ## @publicsection Bind events to the text widget.
            # @param ... Arguments passed to bind.
            ::bind $text {*}$args
        }
        constructor {args} {
            ## Constructor
            # @param widgetpath
            # @param ... Options:
            # @arg -label Label for frame
            # @arg -auto  Passed to Scrolled Window
            # @arg -scrollbar Passed to Scrolled Window
            # @arg -text Get/Set text in text widget
            # @par any options a text widget takes except -xscrollcommand and 
            # -yscrollcommand
            # All text methods (except xview and yview) are available as well.
            set auto [from args -auto]
            set scrollbar [from args -scrollbar]
            installhull using labelframe -class LabeledScrolledText
            install scroll using ScrolledWindow $win.scroll \
                  -auto $auto -scrollbar $scrollbar
            pack $scroll -fill both -expand yes
            install text using text [$scroll getframe].text
            $scroll setwidget $text
            $self configurelist  $args
        }
    }
    snit::macro ::RolePlayingDB3::OpenFilename {{optionspec {-openfilename openFilename OpenFilename}}} {
        ## Macro to add a readonly file option.
        # @param optionspec The option spec.
        option $optionspec \
              -readonly yes -default {} -validatemethod validateinputfile
        method validateinputfile {option value} {
            if {"$value" eq ""} {return "$value"}
            if {[file exists $value] && 
                [file readable $value] && 
                [file isfile $value]} {
                return $value
            } else {
                error "Expected a valid input file for $option, but got $value"
            }
        }
    }
    snit::macro ::RolePlayingDB3::SaveFilename {{optionspec {-savefilename saveFilename SaveFilename}}} {
        ## Macro to add a read/write file option.
        # @param optionspec The option spec. 
        option $optionspec \
              -readonly yes -default {} -validatemethod validateoutputfile
        method validateoutputfile {option value} {
            if {"$value" eq ""} {return "$value"}
            if {(([file exists $value] && 
                  [file writable $value]) || 
                  ([file exists [file dirname $value]] && 
                   [file writable [file dirname $value]])) &&
                ![file isdirectory $value]} {
                return $value
            } else {
                error "Expected a valid output file for $option, but got $value"
            }
        }
    }
    snit::widgetadaptor XMLContentEditor {
        ## Widget to for editing XML Content.
        # Takes a XML string and builds a widget to edit it.
        
        option -xml -readonly yes -default {}
        option {-templatevariable templateVariable TemplateVariable} -default {} \
              -readonly yes
        option {-dirtyvariable dirtyVariable DirtyVariable} -default {}
        method setdirty {} {
            ## @privatesection Set the parent dirty flag (if any)
            #puts stderr "*** $self setdirty"
            if {"$options(-dirtyvariable)" ne ""} {
                upvar #0 "$options(-dirtyvariable)" isdirty
                set isdirty yes
            }
        }
        option {-basedirectory baseDirectory BaseDirectory} -default {}
        option {-filewidgethandler fileWidgetHandler FileWidgetHandler} -default {}
        ::RolePlayingDB3::SaveFilename {-xmlfile xmlFile XmlFile}
        option {-buttoncommand buttonCommand ButtonCommand} -default {}
        option {-isnewobject isNewObject IsNewObject} -readonly yes -default no \
              -type snit::boolean
        option {-rootcontainer rootContainer RootContainer} -readonly yes
        component editframe
        component parsedXML
        variable fileWidgets -array {}
        variable generatedTemplate {}
        variable idmap -array {}
        variable pageno 0
        variable lineno 1000
        variable _widgets -array {}
        variable _xmlnodes -array {}
        method getElementWidgetById {id} {
            ## @publicsection Get an element's widget, given the element's id.
            # @param id the element id.
            # @returns a widget or {}.
            if {[catch {set idmap($id)} w]} {
                return {}
            } else {
                return $w
            }
        }
        method getpageno {} {
            ## Return page number
            # @returns the page number.
            return $pageno
        }
        method getlineno {} {
            ## Return the line number.
            # @returns the line number.
            return $lineno
        }
        method bcmdmethod {{id ""}} {
            ## Button command method
            # @param id Button id
            
            if {$options(-buttoncommand) ne ""} {
                uplevel \#0 "$options(-buttoncommand) $id"
            }
        }
        constructor {args} {
            ## @publicsection Constructor
            # @param widgetpath The widget path.
            # @param ... Options:
            # @arg -xml The XML string (Required).
            # @arg -templatevariable The template variable.
            # @arg -dirtyvariable The dirty variable.
            # @arg -basedirectory The base directory.
            # @arg -filewidgethandler The file widget handler.
            # @arg -xmlfile The file to save the XML to.
            # @arg -buttoncommand The button callback.
            # @arg -isnewobject Flag to indicate that this is a new entity.
            # @arg -rootcontainer The root container's tag.
            
            set options(-xml) [from args -xml]
            if {"$options(-xml)" eq ""} {
                error "The -xml option is a required option!"
            }
            installhull using ScrolledWindow -scrollbar vertical -auto vertical
            install editframe using ScrollableFrame [$hull getframe].editframe \
                  -constrainedwidth yes
            $hull setwidget $editframe
            $self configurelist $args
            install parsedXML using ParseXML %AUTO% $options(-xml)
            set xmlcontainer [$parsedXML getElementsByTagName $options(-rootcontainer) \
                              -depth 1]
            set lf [labelframe [$editframe getframe].container -text [$xmlcontainer data]]
            pack $lf -fill both -expand yes 
            set _widgets($xmlcontainer) $lf
            set _xmlnodes($lf) $xmlcontainer
            $self buildGUI $xmlcontainer $lf
            if {"$options(-templatevariable)" ne ""} {
                upvar #0 "$options(-templatevariable)" template
                set template $generatedTemplate
            }
            if {"$options(-xmlfile)" ne ""} {$self recreateXML "$options(-xmlfile)"}
        }
        destructor {
            ## Destory the XML tree (free up memory).
            $parsedXML destroy
        }
        method buildGUI {container frame} {
            ## @privatesection Build the GUI
            # @param container XML DOM tree root
            # @param frame Outer edit frame
            
            foreach c [$container children] {
                $self makewidget $c $frame
            }
        }
        typevariable genindex 0
        typemethod genname {class} {
            ## Generate a unique symbol name.
            # @param class The class (prefix) for the symbol name.
            # @returns a new unique symbol name.
            
            incr genindex
            return [format {%s%05d} $class $genindex]
        }
        method makewidget {node parentframe} {
            ## Make a Widget for a XML node.
            # @param node The XML node in the DOM tree.
            # @param parentframe The parent frame.
            
            # puts stderr "*** [list $self makewidget $node $parentframe]"
            set nodename [$node cget -tag]
            set tag $nodename
            set attrlist [$node cget -attributes]
            set widget none
            set bindscript {}
            set label  {}
            set widgetopts [list]
            set packopts [list]
            set widgetdata [string trim [$node data]]
            array unset attrlist_array
            array set attrlist_array $attrlist
            if {![catch {set attrlist_array(side)} side]} {
                lappend packopts -side $side
            }
            switch -exact [string totitle $nodename] {
                Li {
                    set curnode $parentframe
                    $curnode insert end -text $widgetdata \
                          -values [list $widgetdata $attrlist]
                    return {}
                }
                Field {
                    if {[catch {set attrlist_array(fill)} fill]} {
                        lappend packopts -fill x
                    } else {
                        lappend packopts -fill $fill
                    }
                    if {![catch {set attrlist_array(expand)} expand]} {
                        lappend packopts -expand $expand
                    }
                    if {[catch {set attrlist_array(name)}]} {
                        set attrlist_array(name) "Unamed Field"
                    }
                    if {[catch {set attrlist_array(generator)}]} {
                        set attrlist_array(generator) {}
                    }
                    if {[catch {set attrlist_array(range)}]} {
                        set attrlist_array(range) {}
                    }
                    if {[catch {set attrlist_array(updatable)}]} {
                        set attrlist_array(updatable) yes
                    }
                    set nodename "Field:$attrlist_array(name)"
                    set label $attrlist_array(name)
                    lappend widgetopts -label "$label:"
                    switch -exact -- "$attrlist_array(type)" {
                        {Whole Number} {
                            if {!$attrlist_array(updatable) && !$options(-isnewobject)} {
                                set widget LabelEntry
                                lappend widgetopts -editable no
                            } else {
                                set widget LabelSpinBox
                                lappend widgetopts -modifycmd [mymethod setdirty]
                                set bindscript [list bind <KeyPress> [mymethod setdirty]]
                                if {[regexp {^([[:digit:]]*)[dD]([[:digit:]]*)$} \
                                     "$attrlist_array(generator)" -> num sides] > 0} {
                                    lappend widgetopts -range [list $num [expr {$sides * $num}] 1]
                                    set default $num
                                } elseif {[regexp {^[dD]%$} "$attrlist_array(generator)"] > 0} {
                                    lappend widgetopts -range {0 99 1}
                                    set default 0
                                } else {
                                    set range $attrlist_array(range)
                                    if {[llength $range] > 1 && [llength $range] < 4} {
                                        lappend widgetopts -range $range -text 0
                                    } else {
                                        lappend widgetopts -range {-9999999 9999999 1} -text 0
                                    }
                                    set default 0
                                    if {[lindex $range 0] > $default} {
                                        set default [lindex $range 0]
                                    }
                                    if {[lindex $range 1] < $default} {
                                        set default [lindex $range 1]
                                    }
                                }
                                lappend widgetopts -text $default
                            }
                        }
                        {Word / Short Phrase} {
                            set widget LabelEntry
                            if {!$attrlist_array(updatable) && !$options(-isnewobject)} {
                                lappend widgetopts -editable no
                            } else {
                                set bindscript [list bind <KeyPress> [mymethod setdirty]]
                            }
                        }
                        Color {
                            set widget LabelSelectColor
                            if {!$attrlist_array(updatable) && !$options(-isnewobject)} {
                                set widget LabelEntry
                                lappend widgetopts -editable no
                            } else {
                                lappend widgetopts -modifycmd [mymethod setdirty]
                            }
                        }
                        {Enumerated Type} {
                            set widget LabelComboBox
                            lappend widgetopts -editable no
                            if {!$attrlist_array(updatable) && !$options(-isnewobject)} {
                                set widget LabelEntry
                            } else {
                                set bindscript [list bind <KeyPress> [mymethod setdirty]]
                                lappend widgetopts -values $attrlist_array(values)
                                lappend widgetopts -modifycmd [mymethod setdirty]
                                lappend widgetopts -text [lindex $attrlist_array(values) 0]
                            }
                        }	     
                        {Long Text} {
                            set widget ::RolePlayingDB3::LabeledScrolledText
                            lappend widgetopts -auto vertical -scrollbar vertical \
                                  -wrap word -width 70 -height 10 -relief sunken
                            set bindscript [list bind <KeyPress> [mymethod setdirty]]
                        }
                        Graphic {
                            set widget ::RolePlayingDB3::Graphic
                            lappend widgetopts -modifycmd [mymethod setdirty] \
                                  -basedirectory $options(-basedirectory) \
                                  -initialdir [::RolePlayingDB3::Configuration \
                                               getoption Imagedir]
                        }
                            Document {
                                set widget FileEntry
                                set bindscript [list bind <KeyPress> [mymethod setdirty]]
                                lappend widgetopts -modifycmd [mymethod setdirty]
                                lappend widgetopts -initialdir [::RolePlayingDB3::Configuration \
                                                                getoption Docdir]
                                lappend widgetopts -filetypes { 
                                {"All Files"        *                   } }
                        }
                    }
                }
                Canvas {
                    if {[catch {set attrlist_array(fill)} fill]} {
                        lappend packopts -fill both
                    } else {
                        lappend packopts -fill $fill
                    }
                    if {[catch {set attrlist_array(expand)} expand]} {
                        lappend packopts -expand yes
                    } else {
                        lappend packopts -expand $expand
                    }
                    set widget ::RolePlayingDB3::ScrolledCanvas
                    if {[catch {set attrlist_array(background)} background]} {
                        lappend widgetopts -background white
                    } else {
                        lappend widgetopts -background $background
                    }
                    #	  puts stderr "*** $self makewidget: tag = $tag, widget = $widget, packopts = $packopts, widgetopts = $widgetopts"
                }
                List {
                    if {[catch {set attrlist_array(fill)} fill]} {
                        lappend packopts -fill both
                    } else {
                        lappend packopts -fill $fill
                    }
                    if {[catch {set attrlist_array(expand)} expand]} {
                        lappend packopts -expand yes
                    } else {
                        lappend packopts -expand $expand
                    }
                    if {[catch {set attrlist_array(selectmode)} selectmode]} {
                        lappend widgetopts -selectmode browse
                    } else {
                        if {$selectmode eq "single"} {set selectmode browse}
                        if {$selectmode eq "multiple"} {set selectmode extended}
                        lappend widgetopts -selectmode $selectmode
                    }
                    set widget ::RolePlayingDB3::ScrolledList
                }
                Button {
                    set widget Button
                    if {[catch {set attrlist_array(label)} label] ||
                        [catch {set attrlist_array(name)}  label]} {
                        set label $nodename
                    }
                    lappend widgetopts -text "$label"
                    if {[catch {set attrlist_array(id)} id]} {set id "$label"}
                    lappend widgetopts -command [mymethod bcmdmethod $id]
                }
                Buttonbox {
                    set widget ButtonBox
                    if {![catch {set attrlist_array(orient)} orient]} {
                        lappend widgetopts -orient $orient
                    }
                    if {![catch {set attrlist_array(homogeneous)} homogeneous]} {
                        lappend widgetopts -homogeneous $homogeneous
                    }
                    if {[catch {set attrlist_array(fill)} fill]} {
                        lappend packopts -fill both
                    } else {
                        lappend packopts -fill $fill
                    }
                    if {[catch {set attrlist_array(expand)} expand]} {
                        lappend packopts -expand yes
                    } else {
                        lappend packopts -expand $expand
                    }
                    #	  puts stderr "*** $self makewidget: tag = $tag, widget = $widget, packopts = $packopts, widgetopts = $widgetopts"
                }
                Bi {
                    set curnode $parentframe
                    set buttonname [$type genname button]
                    if {[winfo class $curnode] ne "ButtonBox"} {
                        error "Illformed XML: Bi nodes can only be children of ButtonBox nodes!"
                    }
                    #	  parray attrlist_array
                    if {![catch {set attrlist_array(label)} label]} {
                        lappend widgetopts -text "$label"
                    } elseif {![catch {set attrlist_array(name)}  label]} {
                        lappend widgetopts -text "$label"
                    } else {
                        lappend widgetopts -text $nodename
                    }
                    if {![catch {set attrlist_array(name)} name]} {
                        set buttonname [string tolower [regsub {[[:space:]]} $name {_}]]
                        #lappend widgetopts -name $name
                    }
                    if {[catch {set attrlist_array(id)} id]} {
                        lappend widgetopts -command [mymethod bcmdmethod "$label"]
                        set id {}
                    } else {
                        lappend widgetopts -command [mymethod bcmdmethod "$id"]
                    }
                    #	  puts stderr "*** $self makewidget: curnode = $curnode, tag = $tag, widgetopts = $widgetopts"
                    set w [eval [list $curnode add ttk::button $buttonname] $widgetopts]
                    if {"$id" ne ""} {
                        set idmap($id) $w
                    }
                    return {}
                }
                default {
                    if {[catch {set attrlist_array(fill)} fill]} {
                        lappend packopts -fill both
                    } else {
                        lappend packopts -fill $fill
                    }
                    if {[catch {set attrlist_array(expand)} expand]} {
                        lappend packopts -expand yes
                    } else {
                        lappend packopts -expand $expand
                    }
                    set widget labelframe
                    lappend widgetopts -text $nodename
                }
            }
            regsub -all {[[:space:]]} $nodename {} wname
            if {[winfo class $parentframe] eq "Labelframe"} {
                set curroot $parentframe
            } else {
                set curroot [$parentframe getframe]
            }
            set wname $curroot.[string tolower $wname]
            set bwname $wname
            set i 0
            while {[winfo exists $wname]} {
                incr i
                set wname $bwname$i
            }
            #puts stderr "*** $self makewidget: wname = $wname, tag = $tag, widget = $widget, packopts = $packopts, widgetopts = $widgetopts"
            eval [list $widget $wname] $widgetopts
            #      if {"$widget" eq "::RolePlayingDB3::ScrolledList"} {
            #	puts stderr "*** $self makewidget: wname = $wname, $wname cget -selectmode = [$wname cget -selectmode]"
            #      }
            #      puts stderr "*** $self makewidget: winfo exists $wname = [winfo exists $wname]"
            if {"$widget" eq "FileEntry" ||
                "$widget" eq "::RolePlayingDB3::Graphic"} {
                set fileWidgets($wname) true
            }
            eval [list pack $wname] $packopts
            if {![catch {set attrlist_array(id)} id]} {
                set idmap($id) $wname
            }
            if {"$bindscript" ne ""} {eval $wname $bindscript}
            #puts stderr "*** $self makewidget: [winfo class $wname] <= '$widgetdata'"
            if {$widgetdata ne {}} {
                catch {$wname configure -text $widgetdata}
            }
            set _widgets($node) $wname
            set _xmlnodes($wname) $node
            foreach c [$node children] {
                $self makewidget $c $wname
            }
        }
        method recreateXML {file} {
            ## @publicsection Regenerate the XML string and save it to a file.
            # @param file The name of the file to write to.
            
            if {[catch {open $file w} shfp]} {
                tk_messageBox -type ok -icon error -message "Internal error: cannot create $file: $shfp"
                return
            }
            puts $shfp {<?xml version="1.0" ?>}
            foreach w [array names _widgets] {
                switch [winfo class $_widgets($w)] {
                    ScrolledList {
                        # lists
                        set parentnode $_xmlnodes($_widgets($w))
                        foreach c [$parentnode children] {
                            $parentnode removeChild $c
                            $c destroy
                        }
                        foreach item [$_widgets($w) items] {
                            lassign [$_widgets($w) itemcget $item -values] descr attrlist
                            set child [SimpleDOMElement create %AUTO% \
                                       -tag Li \
                                       -attributes $attrlist \
                                       -opts [list -namespace http://www.deepsoft.com/roleplayingdb/v3xmlns]]
                            $child setdata $descr
                            $parentnode addchild $child
                        }
                    }
                    ButtonBox -
                    ScrolledCanvas {
                        # nothing here
                    }
                    default {
                        # everything else should just a text data of some sort
                        if {[catch {$w setdata [$_widgets($w) cget -text]}]} {
                            puts stderr "$_widgets($w) cget -text failed: [winfo class $_widgets($w)]"
                        }
                    }
                }
            }
            #$self _recreateXML_processNodesAt $editframe $shfp
            $parsedXML displayTree $shfp {} -addnamespace yes
            close $shfp
        }
        method outputXMLToPDF {pdfobj {heading "Sheet"} {curpage 0} {curline 1000}} {
            ## Output the XML tree as PDF to a pdfobject.
            # @param pdfobj The pdfobject to write to.
            # @param heading The main heading.
            # @param curpage The current page number.
            # @param curline The current line number.
            
            $pdfobj setFont 10 Courier
            $pdfobj setLineSpacing [expr {14.0 / 10.0}]
            set pageno $curpage
            set lineno $curline
            foreach w [array names _widgets] {
                catch {$_widgets($w) setdata [$w cget -text]}
            }
            set xmlcontainer [$parsedXML getElementsByTagName $options(-rootcontainer) \
                              -depth 1]
            $self _outputXMLToPDF_processNodesAt $heading "" $xmlcontainer $pdfobj
        }
        method newPDFPage {pdfobj heading subheading} {
            ## @privatesection Start a new page
            # @param pdfobj The pdfobject to write to.
            # @param heading The main heading.
            # @param subheading The subheading.
            
            incr pageno
            set lineno 1
            $pdfobj startPage
            set width [lindex [$pdfobj getDrawableArea] 0]
            $pdfobj text "$subheading" -x 0 -y 0
            $pdfobj text "$heading" -align center -x [expr {$width/2.0}] -y 0
            $pdfobj text "Page $pageno" -align right -x $width -y 0
            $pdfobj setTextPosition 0 [expr {$lineno * 14}]
            ::RolePlayingDB3::PrintDialog printprogress setpageno $pageno
        }
        proc _fitString {pdfobj line twidth} {
            ## Split a string to fit a given text width.
            # @param pdfobj The pdfobject to write to.
            # @param line The text line to spilt.
            # @param twidth The text width to fit.
            # @returns a list of two strings, the first fits the available 
            # space and the second is whatever is left.
            
            regexp -indices {([[:space:]]+)|$} $line wordend
            while {[$pdfobj getStringWidth [string range $line 0 \
                                            [expr {[lindex $wordend 0] -1}]]] < \
                      $twidth} {
                set e1 [expr {[lindex $wordend 0]-1}]
                set s1 [expr {[lindex $wordend 1]+1}]
                #puts stderr "*** ::_fitString: s1 is $s1, line is [string length $line] long"
                if {$s1 >= [string length $line]} {break}
                regexp -indices -start $s1 {([[:space:]]+)|$} $line wordend
                #puts stderr "*** ::_fitString: wordend is $wordend"
            }
            return [list [string range $line 0 $e1] [string range $line $s1 end]]
        }
        method _outputXMLToPDF_processNodesAt {heading subheading node pdfobj {indent 0}} {
            ## Process one XML node, and then process its children (if any)
            # @param heading The main heading.
            # @param subheading The subheading.
            # @param node The current XML node in the DOM tree.
            # @param pdfobj The pdfobject to write to.
            # @param indent The current indentation.
            
            set tag [$node cget -tag]
            set data [$node data]
            set attrlist [$node cget -attributes]
            array unset attrlist_array
            array set attrlist_array $attrlist
            if {[catch {set attrlist_array(name)} label]} {
                set label [string totitle $tag]
            }
            switch -exact [string totitle "$tag"] {
                Field {
                    switch -exact -- "$attrlist_array(type)" {
                        {Long Text} {
                            set dheight [expr {[lindex [$pdfobj getDrawableArea] 1] - ($lineno * 14)}]
                            if {$pageno < 1 || $dheight < 70} {
                                $self newPDFPage $pdfobj $heading "$subheading"
                            }
                            $pdfobj text "$label:" -x $indent
                            $pdfobj newLine
                            incr lineno
                            set y [expr {$lineno * 14}]
                            set bottom [lindex [$pdfobj getDrawableArea] 1]
                            set thetext $data
                            set twidth [expr {[lindex [$pdfobj getDrawableArea] 0] - $indent}]
                            foreach line [split $thetext "\n"] {
                                while {true} {
                                    #puts stderr "*** $self _outputXMLToPDF_processNodesAt (Long Text): line is '$line'"
                                    lassign [_fitString $pdfobj $line $twidth] l1 l2
                                    if {($y + 14) >= $bottom} {
                                        $self newPDFPage $pdfobj $heading "$subheading"
                                        set y [expr {$lineno * 14}]
                                    }
                                    $pdfobj text $l1 -x $indent
                                    $pdfobj newLine
                                    incr lineno
                                    set y [expr {$lineno * 14}]
                                    set line $l2
                                    if {$l2 eq ""} {break}
                                }
                            }
                        }
                        Graphic {
                            set image [$_widgets($node) getimage]
                            set imwidth [image width $image]
                            set imheight [image height $image]
                            set dwidth [expr {[lindex [$pdfobj getDrawableArea] 0] - $indent}]
                            set dheight [expr {[lindex [$pdfobj getDrawableArea] 1] - ($lineno * 14)}]
                            if {$dwidth < $imwidth} {
                                set wscale [expr {double($dwidth) / double($imwidth)}]
                            } else {
                                set wscale 1.0
                            }
                            if {([lindex [$pdfobj getDrawableArea] 1] - 42) < $imheight} {
                                set hscale [expr {double([lindex [$pdfobj getDrawableArea] 1] - 42) / double($imheight)}]
                            } else {
                                set hscale 1.0
                            }
                            if {$wscale < $hscale} {
                                set scale $wscale
                            } else {
                                set scale $hscale
                            }
                            set height  [expr {int($imheight  * $scale)}]
                            #		puts stderr "*** $self _outputXMLToPDF_processNodesAt: height = $height, imheight = $imheight"
                            #		puts stderr "*** $self _outputXMLToPDF_processNodesAt: pageno = $pageno, dheight = $dheight"
                            if {$pageno < 1 || ($height + 14) > $dheight} {
                                $self newPDFPage $pdfobj "$heading" "$subheading"
                            }
                            #		puts stderr "*** $self _outputXMLToPDF_processNodesAt: lineno = $lineno"
                            $pdfobj text "$label:" -x $indent
                            $pdfobj newLine
                            incr lineno
                            $pdfobj putRawImage [$image data] $indent [expr {$lineno * 14}] -height $height
                            set lines [expr {int(ceil(double($height) / 14.0))}]
                            for {set i 0} {$i < $lines} {incr i} {$pdfobj newLine}
                            incr lineno $lines
                            #		puts stderr "*** $self _outputXMLToPDF_processNodesAt: lineno = $lineno (after Graphic)"
                        }
                        default {
                            set text  $data
                            #puts stderr "*** $self _outputXMLToPDF_processNodesAt: pageno = $pageno, lineno = $lineno"
                            if {$pageno < 1 || [expr {$lineno * 14}] > [lindex [$pdfobj getDrawableArea] 1]} {
                                $self newPDFPage $pdfobj $heading "$subheading"
                            }
                            $pdfobj text "$label: $text" -x $indent
                            $pdfobj newLine
                            incr lineno
                            #puts stderr "*** $self _outputXMLToPDF_processNodesAt: lineno = $lineno (after Other field)"
                        }
                    }
                }
                List {
                    foreach c [$node children] {
                        set text [$c data]
                        #puts stderr "*** $self _outputXMLToPDF_processNodesAt: pageno = $pageno, lineno = $lineno"
                        if {$pageno < 1 || [expr {$lineno * 14}] > [lindex [$pdfobj getDrawableArea] 1]} {
                            $self newPDFPage $pdfobj $heading "$subheading"
                        }
                        $pdfobj text " * $text" -x $indent
                        $pdfobj newLine
                        incr lineno
                        #puts stderr "*** $self _outputXMLToPDF_processNodesAt: lineno = $lineno (after List element)"
                    }
                }
                Canvas {
                    set dheight [expr {[lindex [$pdfobj getDrawableArea] 1] - ($lineno * 14)}]
                    set dwidth [expr {[lindex [$pdfobj getDrawableArea] 0] - $indent}]
                    set bbox [$_widgets($node) bbox all]
                    set cw [expr {[lindex $bbox 2] - [lindex $bbox 0]}]
                    set ch [expr {[lindex $bbox 3] - [lindex $bbox 1]}]
                    if {$dwidth < $cw} {
                        set wscale [expr {double($dwidth) / double($cw)}]
                    } else {
                        set wscale 1.0
                    }
                    if {([lindex [$pdfobj getDrawableArea] 1] - 56) < $ch} {
                        set hscale [expr {double([lindex [$pdfobj getDrawableArea] 1] - 56) / double($ch)}]
                    } else {
                        set hscale 1.0
                    }
                    if {$wscale < $hscale} {
                        set scale $wscale
                    } else {
                        set scale $hscale
                    }
                    set height  [expr {int($ch  * $scale)}]
                    if {$pageno < 1 || $height > $dheight} {
                        $self newPDFPage $pdfobj "$heading" "$subheading"
                    }
                    $pdfobj canvas $_widgets($node) -x $indent -y [expr {$lineno * 14}] \
                          -height $height -bg yes
                    set lines [expr {int(ceil(double($height) / 14.0))+2}]
                    for {set i 0} {$i < $lines} {incr i} {$pdfobj newLine}
                    incr lineno $lines
                }
                Button -
                Buttonbox {
                }
                default {
                    set subheading $data
                    set dheight [expr {[lindex [$pdfobj getDrawableArea] 1] - ($lineno * 14)}]
                    #	    puts stderr "*** $self _outputXMLToPDF_processNodesAt: pageno = $pageno, lineno = $lineno"
                    if {$pageno < 1 || $dheight < 28} {
                        $self newPDFPage $pdfobj "$heading" "$subheading"
                    }
                    $pdfobj setFont 10 Courier-Bold
                    $pdfobj text "$subheading" -x $indent
                    $pdfobj newLine
                    incr lineno
                    #	    puts stderr "*** $self _outputXMLToPDF_processNodesAt: lineno = $lineno (after Container)"
                    $pdfobj setFont 10 Courier
                    foreach c [$node children] {
                        $self _outputXMLToPDF_processNodesAt "$heading" "$subheading" $c $pdfobj [expr {$indent + 24}]
                    }
                }
            }
        }
        typemethod ExtractTagValue {xmlstring name default} {
            ## @publicsection Fetch the value (character data) of a given 
            # Field by name in a XML String.
            # @param xmlstring The XML string.
            # @param name The name.
            # @param default The value to return if the named field is not 
            # found.
            
            set result $default
            set xml [ParseXML %AUTO% $xmlstring]
            set fields [$xml getElementsByTagName Field]
            foreach f $fields {
                if {[$f attribute name] eq "$name"} {
                    set result [$f data]
                    break
                }
            }
            $xml destroy
            return $result
        }
    }
    snit::macro ::RolePlayingDB3::GeneratePrintDialog {nameprefix additionalcomps createbody} {
        ## Macro to generate code to create a print dialog.
        # @param nameprefix Dialog name prefix
        # @param additionalcomps Additional components
        # @param createbody Additional dialog create body.
        
        typecomponent _printdialog
        typecomponent   printfileFE
        typecomponent   papersizeLCB
        foreach c $additionalcomps {
            typecomponent $c
        }
        typevariable printerIcon
        typevariable printerfiletypes { {{PDF Files} {.pdf}       }
            {{All Files} *            } }
        regsub {%%nameprefix%%} {
            if {"$_printdialog" ne "" && [winfo exists $_printdialog]} {return}
            set _printdialog [Dialog .%%nameprefix%%printdialog -image $printerIcon \
                              -cancel 1 -default 0 -modal local \
                              -parent . -side bottom \
                              -title "Print" -transient yes]
            $_printdialog add print   -text {Print}
            $_printdialog add cancel  -text {Cancel}
            set frame [$_printdialog getframe]
            set printfileFE [FileEntry $frame.printfileFE -label "Output file:" \
                             -labelwidth 12 \
                             -filetypes $printerfiletypes \
                             -filedialog save]
            pack $printfileFE -fill x
            set papersizeLCB [LabelComboBox $frame.papersizeLCB -label "Paper size:" \
                              -labelwidth 12 \
                              -editable no \
                              -values [::pdf4tcl::getPaperSizeList]]
            pack $papersizeLCB -fill x
            $papersizeLCB set [lindex [$papersizeLCB cget -values] 0]
        } "$nameprefix" createPrintDialogBody
        append createPrintDialogBody $createbody
        typemethod createPrintDialog {} $createPrintDialogBody
        typemethod drawPrintDialog {args} {
            $type createPrintDialog
            set parent [from args -parent .]
            $_printdialog configure -parent $parent
            set what [from args -what "Sheet"]
            $_printdialog configure -title "Print $what"
            set filename [from args -filename printout.pdf]
            $printfileFE configure -text "$filename"
            set ans [$_printdialog draw]
            switch $ans {
                0 {
                    set result [::pdf4tcl::new %AUTO% -paper [$papersizeLCB get] \
                                -file  [$printfileFE  get] \
                                -margin 36]
                    ::RolePlayingDB3::PrintDialog printprogress start
                }
                1 {
                    set result {}
                }
            }
            return $result
        }
    }
    snit::type PrintDialog {
        ## General purpose Print Dialog
        # This is a general purpose print dialog.
        pragma -hastypedestroy no
        pragma -hasinstances no
        pragma -hastypeinfo no
        typecomponent _printProgressDialog
        ## @privatesection Progress dialog
        typecomponent   printprogressPageNoLE
        ## Current page number
        typeconstructor {
            ## Type constructor: Initialize type variables/components.
            
            set printerIcon [image create photo \
                             -file [file join $::RolePlayingDB3::ImageDir \
                                    largePrinter.gif]]
            set _printdialog {}
            set _printProgressDialog {}
        }
        ::RolePlayingDB3::GeneratePrintDialog {} {} {}
        typemethod create_printProgressDialog {} {
            ## Create the progress dialog.
            if {"$_printProgressDialog" ne "" &&
                [winfo exists "$_printProgressDialog"]} {return}
            set _printProgressDialog [Dialog .printProgressDialog \
                                      -image $printerIcon -default 0 \
                                      -modal none -parent . -side bottom \
                                      -title "Print Progress" -transient yes]
            $_printProgressDialog add dismis -text {Dismis} -state disabled \
                  -command [mytypemethod _DismisPrintProgress]
            set frame [$_printProgressDialog getframe]
            set printprogressPageNoLE [LabelEntry $frame.printprogressPageNoLE \
                                       -label {Printing Page:} \
                                       -text "" -editable no]
            pack $printprogressPageNoLE -fill x
        }
        typevariable _oldfocus
        ## Holds old focus.
        typevariable _oldgrab
        ## Holds old grab.
        typemethod {printprogress start} {args} {
            ## @publicsection Start the progress dialog.
            # @param ... Options:
            # @arg -parent Parent window.
            
            set parent [from args -parent .]
            $type create_printProgressDialog
            $printprogressPageNoLE configure -text ""
            $_printProgressDialog configure -parent $parent
            wm transient [winfo toplevel $_printProgressDialog] $parent
            set _oldfocus [focus -displayof $printprogressPageNoLE]
            focus $printprogressPageNoLE
            set _oldgrab [grab current $_printProgressDialog]
            grab set $_printProgressDialog
            $_printProgressDialog itemconfigure dismis -state disabled
            $_printProgressDialog draw
        }
        typemethod {printprogress setpageno} {pageno} {
            ## Set the current print progress page number.
            # @param pageno New current page number.
            
            $printprogressPageNoLE configure -text "$pageno"
            update idle
        }
        typemethod {printprogress end} {} {
            ## End the progress dialog: release grab and focus and enable the 
            # dismis button.
            
            $_printProgressDialog itemconfigure dismis -state normal
            grab release $_printProgressDialog
            if {$_oldgrab ne ""} {grab set $_oldgrab}
            focus $_oldfocus
        }
        typemethod _DismisPrintProgress {} {
            ## @privatesection Dismis the print progress dialog.
            $_printProgressDialog withdraw
        }
    }
    snit::widget chroot_chooseDirectory {
        ## Chrooted version of tk_chooseDirectory.
        # (copied and adapted from tk_chooseDirectory).
        # This version is "chrooted" to the vfs holding a RPG bundle.
        
        hulltype toplevel
        delegate option -cursor to hull
        option {-initialdir initialDir InitialDir} -default {}
        option -root -default / -validatemethod validexistingdirectory
        method validexistingdirectory {option value} {
            if {[file exists $value] && [file isdirectory $value]} {
                return $value
            }
            error "Expected a valid existing directory name for $option, got $value"
        }
        option -title -default "Select a directory"
        delegate option {-bannerimage bannerImage BannerImage} to banner as -image
        delegate option {-bannerbackground bannerBackground BannerBackground} to banner as -background
        option -parent -default .
        option {-mustexist mustExist MustExist} -default no -type snit::boolean
        typevariable updirImage
        typevariable folderImage
        typeconstructor {
            set updirImage [image create bitmap -data {
#define updir_width 28
#define updir_height 16
static char updir_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x80, 0x1f, 0x00, 0x00, 0x40, 0x20, 0x00, 0x00,
   0x20, 0x40, 0x00, 0x00, 0xf0, 0xff, 0xff, 0x01, 0x10, 0x00, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x07, 0x00, 0x01, 0x90, 0x0f, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01,
   0x10, 0xfe, 0x07, 0x01, 0x10, 0x00, 0x00, 0x01, 0x10, 0x00, 0x00, 0x01,
   0xf0, 0xff, 0xff, 0x01};}]
            set folderImage [image create photo -data {
R0lGODlhEAAMAKEAAAD//wAAAPD/gAAAACH5BAEAAAAALAAAAAAQAAwAAAIghINhyycvVFsB
QtmS3rjaH1Hg141WaT5ouprt2HHcUgAAOw==}]
        }
        component banner
        component topframe
        component   dirlabel
        component   dirmenubtn
        component   dirmenu
        component   dirupbutton
        component iconList
        component bottomframe
        component   caption
        component   entry
        component   okButton
        component   cancelButton
        variable selectPath {}
        variable updateId
        variable selectFilePath
        constructor {args} {
            install banner using tk::label $win.banner -anchor w
            pack $banner -fill x
            install topframe using frame $win.topframe
            install dirlabel using tk::label $topframe.dirlabel -text "Directory:" \
                  -underline 0
            set dirmenubtn $topframe.dirmenu
            bind $dirlabel <<AltUnderlined>> [list focus $dirmenubtn]
            install dirmenu using tk_optionMenu $topframe.dirmenu \
                  [myvar selectPath] ""
            install dirupbutton using tk::button $topframe.dirupbutton -image $updirImage
            $dirmenubtn configure -takefocus 1 -highlightthickness 2
            pack $dirupbutton -side right -padx 4 -fill both
            pack $dirlabel -side left -padx 4 -fill both
            pack $dirmenubtn -expand yes -fill both -padx 4
            install iconList using ::tk::IconList $win.iconList \
                  -command [mymethod DblClick] -multiple no
            bind $iconList <<ListboxSelect>> [mymethod ListBrowse]
            install bottomframe using frame $win.bottomframe -bd 0
            install caption using tk::label $bottomframe.caption -text "Selection:" \
                  -underline 0 -anchor e \
                  -pady 0
            bind $caption <<AltUnderlined>> [list focus $bottomframe.entry]
            install entry using tk::entry $bottomframe.entry
            $iconList configure -font [$entry cget -font]
            install okButton using tk::button $bottomframe.okButton -text "OK" \
                  -underline 0 \
                  -default active \
                  -pady 3
            bind $okButton <<AltUnderlined>> [list $okButton invoke]
            install cancelButton  using tk::button $bottomframe.cancelButton \
                  -text "Cancel" \
                  -underline 0 \
                  -default normal \
                  -pady 3
            bind $cancelButton <<AltUnderlined>> [list $cancelButton invoke]
            grid $caption $entry $okButton -padx 4 -sticky ew
            grid configure $entry -padx 2
            grid  x x $cancelButton -padx 4 -sticky ew
            grid columnconfigure $bottomframe  1 -weight 1
            pack $topframe -side top -fill x -pady 4
            pack $bottomframe -side bottom -fill x
            pack $iconList -expand yes -fill both -padx 4 -pady 1
            wm protocol $win WM_DELETE_WINDOW [mymethod CancelCmd]
            $dirupbutton configure -command [mymethod UpDirCmd]
            $cancelButton configure -command [mymethod CancelCmd]
            bind $win <KeyPress-Escape> [mymethod CancelCmd]
            bind $win <Alt-Key> [list tk::AltKeyInDialog $win %A]
            set okCmd [mymethod OkCmd]
            bind $entry <Return> $okCmd
            $okButton configure -command $okCmd
            bind $win <Alt-s> [list focus $entry]
            bind $win <Alt-o> [list tk::ButtonInvoke $okButton]
            ::tk::FocusGroup_Create $win
            ::tk::FocusGroup_BindIn $win $entry [mymethod SelectionFocusIn]
            ::tk::FocusGroup_BindOut $win $entry [mymethod SelectionFocusOut]
            $self configurelist $args
            wm withdraw $win
        }
        method draw {args} {
            $self configurelist $args
            set initialdir $options(-initialdir)
            set initdirpathtype [file pathtype $initialdir]
            if {"$initdirpathtype" eq "absolute" ||
                "$initdirpathtype" eq "volumerelative"} {
                set initialdir [eval [linsert [lrange \
                                               [file split $options(-initialdir)] \
                                               1 end] \
                                               0 file join]]
            }
            if {[winfo viewable [winfo toplevel $options(-parent)]]} {
                wm transient $win $options(-parent)
            }
            if {$initialdir ne "" && [file exists [file join $options(-root) $initialdir]] &&
                [file isdirectory [file join $options(-root) $initialdir]]} {
                set selectPath $initialdir
            }
            trace variable [myvar selectPath] w [mymethod SetPath]
            $dirmenubtn configure -textvariable [myvar selectPath]
            set previousEntryText ""
            $self UpdateWhenIdle
            ::tk::PlaceWindow $win widget $options(-parent)
            wm title $win $options(-title)
            ::tk::SetFocusGrab $win $entry
            $entry delete 0 end
            $entry insert 0 $selectPath
            $entry selection range 0 end
            $entry icursor end
            vwait [myvar selectFilePath]
            ::tk::RestoreFocusGrab $win $entry withdraw
            foreach trace [trace vinfo [myvar selectPath]] {
                trace vdelete [myvar selectPath] [lindex $trace 0] [lindex $trace 1]
            }
            $dirmenubtn configure -textvariable {}
            if {"$selectFilePath" eq ""} {
                return $selectFilePath
            } else {
                return [file join $options(-root) $selectFilePath]
            }
        }
        method UpdateWhenIdle {} {
            if {![info exists updateId]} {
                set updateId [after idle [mymethod Update]]
            }
        }
        method DblClick {} {
            set selection  [$iconList selection get]
            if { [llength $selection] != 0 } {
                set filenameFragment \
                      [$iconList get [lindex $selection 0]]
                set file [file join $options(-root) $selectPath]
                #	puts stderr "*** $self DblClick: file = $file"
                if {[file isdirectory $file]} {
                    $self ListInvoke [list $filenameFragment]
                    return
                }
            }
        }
        method ListInvoke {filenames} {
            if {[llength $filenames] == 0} {
                return
            }
            set file [::tk::dialog::file::JoinFile $selectPath \
                      [lindex $filenames 0]]
            #      puts stderr "*** $self ListInvoke: file = $file"
            #      puts stderr "*** $self ListInvoke: file join $options(-root) $file = [file join $options(-root) $file]"
            if {[file isdirectory [file join $options(-root) $file]]} {
                set selectPath $file
            }
        }
        method ListBrowse {} {
            set items [$iconList selection get]
            if {[llength $items] < 1} {return}
            set text [$iconList get [lindex $items 0]]
            set file [::tk::dialog::file::JoinFile $selectPath $text]
            $entry delete 0 end
            $entry insert 0 $file
        }
        method CancelCmd {} {
            set selectFilePath ""
        }
        method UpDirCmd {} {
            if {"$selectPath" ne ""} {
                #puts stderr "[list *** $self UpDirCmd: selectPath = $selectPath]"
                #puts stderr "[list *** $self UpDirCmd: file dirname $selectPath = [file dirname $selectPath]]"
                set selectPath [file dirname $selectPath]
                if {$selectPath eq "."} {set selectPath ""}
            }
        }
        method OkCmd {} {
            #puts stderr "*** $self OkCmd"
            set selection [$iconList selection get]
            #puts stderr "*** $self OkCmd: \[llength \$selection] is [llength $selection]"
            if { [llength $selection] != 0 } {
                set iconText [$iconList get [lindex $selection 0]]
                set iconText [file join $selectPath $iconText]
                $self Done "$iconText"
            } else {
                set text [$entry get]
                if {"$text" eq ""} {return}
                set text [eval file join [file split [string trim $text]]]
                if { "$text" eq "$selectPath" } {
                    $self Done "$text"
                } else {
                    set selectPath "$text"
                }
            }
            return
        }
        method SelectionFocusIn {} {
            if {[string compare [$entry get] ""]} {
                $entry selection range 0 end
                $entry icursor end
            } else {
                $entry selection clear
            }
        }
        method SelectionFocusOut {} {
            $entry selection clear
        }
        method SetPath {name1 name2 op} {
            #      puts stderr "*** $self SetPath $name1 $name2 $op"
            #      puts stderr "*** $self SetPath: winfo exists $win = [winfo exists $win]"
            if {[winfo exists $win]} {
                $self UpdateWhenIdle
                $entry delete 0 end
                $entry insert end $selectPath
            }
        }
        method Update {} {
            if {![winfo exists $win]} {return}
            catch {unset updateId}
            set entCursor [$entry cget -cursor]
            set dlgCursor [$win       cget -cursor]
            $entry configure -cursor watch
            $win       configure -cursor watch
            update idletasks
            #puts stderr "*** $self Update: selectPath = $selectPath"
            set lookupdir [file join $options(-root) $selectPath]
            #puts stderr "*** $self Update: lookupdir is $lookupdir"
            if {![file exists $lookupdir]} {
                $self Done "$lookupdir"
                return
            }
            $iconList deleteall
            #puts stderr "*** $self Update: iconList cleared"
            set unsorteddirs [glob -tails -directory $lookupdir -type d -nocomplain *]
            #puts stderr "*** $self Update: unsorteddirs is $unsorteddirs"
            set dirs [lsort -dictionary -unique $unsorteddirs]
            #puts stderr "[list *** $self Update: dirs = $dirs]"
            set dirList {}
            foreach d $dirs {
                lappend dirList $d
            }
            $iconList add $folderImage $dirList
            set list ""
            set dir ""
            #puts stderr "*** $self Update (just before subdir loop): file split selectPath = [file split $selectPath]"
            foreach subdir [file split $selectPath] {
                puts stderr "*** $self Update (in subdir loop): subdir is $subdir"
                set dir [file join $dir $subdir]
                puts stderr "*** $self Update (in subdir loop): dir is $dir"
                lappend list $dir
                puts stderr "[list *** $self Update (in subdir loop): list = $list]"
            }
            #puts stderr "[list *** $self Update: list = $list]"
            $dirmenu delete 0 end
            #puts stderr "*** $self Update: dirmenu emptied"
            set var [myvar selectPath]
            #puts stderr "*** $self Update: var is \"$var\""
            foreach path $list {
                $dirmenu add command -label $path -command [list set $var $path]
            }
            #puts stderr "*** $self Update: dirmenu updated"
            $entry configure -cursor $entCursor
            $win       configure -cursor $dlgCursor
        }
        method Done {{_selectFilePath ""}} {
            if {[string equal $_selectFilePath ""]} {
                set _selectFilePath $selectPath
            }
            if { $options(-mustexist) } {
                if { ![file exists [file join $options(-root) $_selectFilePath]] || \
                          ![file isdir [file join $options(-root) $_selectFilePath]] } {
                    return
                }
            }
            set selectFilePath $_selectFilePath
        }
    }
    snit::widget chroot_getFile {
        ## Chrooted version of tk_getOpenFile / tk_getSaveFile.
        # (copied and adapted from tk_getOpenFile / tk_getSaveFil).
        # This version is "chrooted" to the vfs holding a RPG bundle.
        
        hulltype toplevel
        delegate option -cursor to hull
        option {-initialdir initialDir InitialDir} -default {}
        option {-initialfile initialFile InitialFile} -default {}
        option {-filetypes fileTypes FileTypes} -default {}
        option {-defaultextension defaultExtension DefaultExtension} -default {}
        option {-saveoropen saveOrOpen SaveOrOpen} \
              -type { snit::enum -values {save open}} \
              -default open
        option -root -default / -validatemethod validexistingdirectory
        method validexistingdirectory {option value} {
            if {[file exists $value] && [file isdirectory $value]} {
                return $value
            }
            error "Expected a valid existing directory name for $option, got $value"
        }
        option -title -default "Select a directory"
        delegate option {-bannerimage bannerImage BannerImage} to banner as -image
        delegate option {-bannerbackground bannerBackground BannerBackground} to banner as -background
        option -parent -default .
        typevariable updirImage
        typevariable folderImage
        typevariable fileImage
        typeconstructor {
            set updirImage [image create bitmap -data {
#define updir_width 28
#define updir_height 16
static char updir_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x80, 0x1f, 0x00, 0x00, 0x40, 0x20, 0x00, 0x00,
   0x20, 0x40, 0x00, 0x00, 0xf0, 0xff, 0xff, 0x01, 0x10, 0x00, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x07, 0x00, 0x01, 0x90, 0x0f, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01,
   0x10, 0xfe, 0x07, 0x01, 0x10, 0x00, 0x00, 0x01, 0x10, 0x00, 0x00, 0x01,
   0xf0, 0xff, 0xff, 0x01};}]
            set folderImage [image create photo -data {
R0lGODlhEAAMAKEAAAD//wAAAPD/gAAAACH5BAEAAAAALAAAAAAQAAwAAAIghINhyycvVFsB
QtmS3rjaH1Hg141WaT5ouprt2HHcUgAAOw==}]
            set fileImage    [image create photo -data {
R0lGODlhDAAMAKEAALLA3AAAAP//8wAAACH5BAEAAAAALAAAAAAMAAwAAAIgRI4Ha+IfWHsO
rSASvJTGhnhcV3EJlo3kh53ltF5nAhQAOw==}]
        }
        component banner
        component topframe
        component   dirlabel
        component   dirmenubtn
        component   dirmenu
        component   dirupbutton
        component iconList
        component bottomframe
        component   caption
        component   entry
        component   typeMenuLab
        component   typeMenuBtn
        component     typeMenu
        component   okButton
        component   cancelButton
        variable selectPath {}
        variable selectFile {}
        variable updateId
        variable selectFilePath
        variable filter "*"
        variable extUsed
        constructor {args} {
            install banner using tk::label $win.banner -anchor w
            pack $banner -fill x
            install topframe using frame $win.topframe
            install dirlabel using tk::label $topframe.dirlabel -text "Directory:" \
                  -underline 0
            set dirmenubtn $topframe.dirmenu
            bind $dirlabel <<AltUnderlined>> [list focus $dirmenubtn]
            install dirmenu using tk_optionMenu $topframe.dirmenu \
                  [myvar selectPath] ""
            install dirupbutton using tk::button $topframe.dirupbutton -image $updirImage
            $dirmenubtn configure -takefocus 1 -highlightthickness 2
            pack $dirupbutton -side right -padx 4 -fill both
            pack $dirlabel -side left -padx 4 -fill both
            pack $dirmenubtn -expand yes -fill both -padx 4
            install iconList using ::tk::IconList $win.iconList \
                  -command [mymethod OkCmd] -multiple no
            bind $iconList <<ListboxSelect>> [mymethod ListBrowse]
            install bottomframe using frame $win.bottomframe -bd 0
            install caption using tk::label $bottomframe.caption -text "File name:" \
                  -underline 5 -anchor e \
                  -pady 0
            bind $caption <<AltUnderlined>> [list focus $bottomframe.entry]
            install entry using tk::entry $bottomframe.entry
            $iconList configure -font [$entry cget -font]
            install typeMenuLab using tk::button $bottomframe.typeMenuLab \
                  -text "Files of type:" -underline 9 -anchor e  \
                  -bd [$caption cget -bd] -relief [$caption cget -relief] \
                  -padx [$caption cget  -padx] -pady [$caption cget  -pady]
            bindtags $typeMenuLab [list $typeMenuLab tk::label \
                                   [winfo toplevel $typeMenuLab] all]
            install typeMenuBtn using menubutton $bottomframe.typeMenuBtn \
                  -indicatoron 1 -menu $bottomframe.typeMenuBtn.m
            install typeMenu using menu $typeMenuBtn.m -tearoff 0
            $typeMenuBtn configure -takefocus 1 -highlightthickness 2 \
                  -relief raised -bd 2 -anchor w
            bind $typeMenuLab <<AltUnderlined>> [list focus $typeMenuBtn]
            install okButton using tk::button $bottomframe.okButton -text "OK" \
                  -underline 0 \
                  -default active \
                  -pady 3
            bind $okButton <<AltUnderlined>> [list $okButton invoke]
            install cancelButton  using tk::button $bottomframe.cancelButton \
                  -text "Cancel" \
                  -underline 0 \
                  -default normal \
                  -pady 3
            bind $cancelButton <<AltUnderlined>> [list $cancelButton invoke]
            grid $caption $entry $okButton -padx 4 -sticky ew
            grid configure $entry -padx 2
            grid $typeMenuLab $typeMenuBtn   $cancelButton -padx 4 -sticky ew
            grid configure $typeMenuBtn -padx 0
            grid columnconfigure $bottomframe  1 -weight 1
            pack $topframe -side top -fill x -pady 4
            pack $bottomframe -side bottom -fill x
            pack $iconList -expand yes -fill both -padx 4 -pady 1
            wm protocol $win WM_DELETE_WINDOW [mymethod CancelCmd]
            $dirupbutton configure -command [mymethod UpDirCmd]
            $cancelButton configure -command [mymethod CancelCmd]
            bind $win <KeyPress-Escape> [mymethod CancelCmd]
            bind $win <Alt-Key> [list tk::AltKeyInDialog $win %A]
            bind $entry <Return> [mymethod ActivateEntry]
            $okButton configure -command [mymethod OkCmd]
            bind $win <Alt-t> [format {
                               if {[string equal [%s cget -state] "normal"]} {
                                   focus %s
                               }
                           } $typeMenuBtn $typeMenuBtn]
            ::tk::FocusGroup_Create $win
            ::tk::FocusGroup_BindIn $win $entry [mymethod SelectionFocusIn]
            ::tk::FocusGroup_BindOut $win $entry [mymethod SelectionFocusOut]
            $self configurelist $args
            wm withdraw $win
        }
        method draw {args} {
            $self configurelist $args
            set initialdir $options(-initialdir)
            set initialfile $options(-initialfile)
            if {"$initialdir" eq ""} {set initialdir [file dirname $initialfile]}
            set initdirpathtype [file pathtype $initialdir]
            if {"$initdirpathtype" eq "absolute" ||
                "$initdirpathtype" eq "volumerelative"} {
                set initialdir [eval [linsert [lrange \
                                               [file split $options(-initialdir)] \
                                               1 end] \
                                               0 file join]]
            }
            set initialfile [file join $initialdir [file tail $initialfile]]
            if {[winfo viewable [winfo toplevel $options(-parent)]]} {
                wm transient $win $options(-parent)
            }
            trace variable [myvar selectPath] w [mymethod SetPath]
            $dirmenubtn configure -textvariable [myvar selectPath]
            # Initialize the file types menu
            #
            if {[llength $options(-filetypes)]} {
                $typeMenu delete 0 end
                foreach ftype $options(-filetypes) {
                    set title  [lindex $ftype 0]
                    set filter [lindex $ftype 1]
                    $typeMenu add command -label $title \
                          -command [mymethod SetFilter $ftype]
                }
                $self SetFilter [lindex $options(-filetypes) 0]
                $typeMenuBtn configure -state normal
                $typeMenuBtn configure -state normal
            } else {
                set filter "*"
                $typeMenuBtn configure -state disabled -takefocus 0
                $typeMenuBtn configure -state disabled
            }
            set previousEntryText ""
            $self UpdateWhenIdle
            ::tk::PlaceWindow $win widget $options(-parent)
            wm title $win $options(-title)
            ::tk::SetFocusGrab $win $entry
            if {$initialfile ne "" && [file exists [file join $options(-root) $initialdir]] &&
                [file isdirectory [file join $options(-root) $initialdir]]} {
                set selectFile $initialfile
                set selectPath [file dirname $initialfile]
                if {$selectPath eq ""} {set selectPath {}}
            }
            $entry delete 0 end
            $entry insert 0 $selectFile
            $entry selection range 0 end
            $entry icursor end
            vwait [myvar selectFilePath]
            ::tk::RestoreFocusGrab $win $entry withdraw
            foreach trace [trace vinfo [myvar selectPath]] {
                trace vdelete [myvar selectPath] [lindex $trace 0] [lindex $trace 1]
            }
            $dirmenubtn configure -textvariable {}
            #puts stderr "*** method draw (after vwait): selectFilePath = $selectFilePath"
            #puts stderr "*** method draw (after vwait): file join $options(-root) $selectFilePath = [file join $options(-root) $selectFilePath]"
            if {"$selectFilePath" eq ""} {
                return $selectFilePath
            } else {
                return [file join $options(-root) $selectFilePath]
            }
        }
        method UpdateWhenIdle {} {
            #puts stderr "*** $self UpdateWhenIdle"
            if {![info exists updateId]} {
                set updateId [after idle [mymethod Update]]
                #puts stderr "*** $self UpdateWhenIdle: new updateId = $updateId"
            } else {
                #puts stderr "*** $self UpdateWhenIdle: old updateId = $updateId"
            }
        }
        method ListInvoke {filenames} {
            if {[llength $filenames] == 0} {
                return
            }
            set file [::tk::dialog::file::JoinFile $selectPath \
                      [lindex $filenames 0]]
            #      puts stderr "*** $self ListInvoke: file = $file"
            #      puts stderr "*** $self ListInvoke: file join $options(-root) $file = [file join $options(-root) $file]"
            if {[file isdirectory [file join $options(-root) $file]]} {
                set appPWD [pwd]
                if {[catch {cd [file join $options(-root) $file]}]} {
                    tk_messageBox -type ok -parent $win  -message [format "Cannot change to the directory %s.\nPermission denied." $file]] -icon warning
                } else {
                    cd $appPWD
                    #puts stderr "*** $self ListInvoke: setting selectPath to $file"
                    set selectPath $file
                }
            } else {
                #puts stderr "*** $self ListInvoke: setting selectFile to $file"
                set selectFile $file
                $self Done
            }
        }
        method ListBrowse {} {
            set items [$iconList selection get]
            if {[llength $items] < 1} {return}
            set text [$iconList get [lindex $items 0]]
            set file [::tk::dialog::file::JoinFile $selectPath $text]
            set isDir [file isdirectory [file join $options(-root) $file]]
            if {!$isDir} {
                $entry delete 0 end
                $entry insert 0 $text
                switch $options(-saveoropen) {
                    open {
                        $okButton configure -text Open -underline 0
                    }
                    save {
                        $okButton configure -text Save -underline 0
                    }
                }
            } else {
                $okButton configure -text Open -underline 0
            }
        }
        method CancelCmd {} {
            set selectFilePath ""
        }
        method UpDirCmd {} {
            if {"$selectPath" ne ""} {
                #puts stderr "[list *** $self UpDirCmd: selectPath = $selectPath]"
                #puts stderr "[list *** $self UpDirCmd: file dirname $selectPath = [file dirname $selectPath]]"
                set selectPath [file dirname $selectPath]
                if {$selectPath eq ""} {set selectPath {}}
            }
        }
        method OkCmd {} {
            #puts stderr "*** $self OkCmd"
            set filenames {}
            foreach item [$iconList selection get] {
                lappend filenames [$iconList get $item]
            }
            #puts stderr "[list *** $self OkCmd: filenames = $filenames]"
            if {[llength $filenames] == 1} {
                set filename [lindex $filenames 0]
                set file [::tk::dialog::file::JoinFile $selectPath $filename]
                #        puts stderr "*** $self OkCmd: file = $file, filename = $filename"
                if {[file isdirectory [file join $options(-root) $file]]} {
                    $self ListInvoke [list $filename]
                    return
                }
            }
            $self ActivateEntry
        }
        method SelectionFocusIn {} {
            if {[string compare [$entry get] ""]} {
                $entry selection range 0 end
                $entry icursor end
            } else {
                $entry selection clear
            }
            switch $options(-saveoropen) {
                open {
                    $okButton configure -text Open -underline 0
                }
                save {
                    $okButton configure -text Save -underline 0
                }
            }
        }
        method SelectionFocusOut {} {
            $entry selection clear
        }
        method ActivateEntry {} {
            set text [$entry get]
            $self VerifyFileName $text
        }
        method VerifyFileName {filename} {
            set list [$self ResolveFile $selectPath $filename $options(-defaultextension)]
            foreach {flag path file} $list {break}
            #      puts stderr "*** $self VerifyFileName: filename is $filename, flag is $flag, path is $path, file is $file"
            switch -- $flag {
                OK {
                    if {[string equal $file ""]} {
                        set selectPath $path
                        $entry delete 0 end
                    } else {
                        $self SetPathSilently $path
                        set selectFile $file
                        $self Done
                    }
                }
                PATTERN {
                    set selectPath $path
                    set filter $file
                }
                FILE {
                    if {[string equal $options(-saveoropen) open]} {
                        tk_messageBox -icon warning -type ok -parent $win \
                              -message "[format {File %s  does not exist.} [file join $path $file]]"
                        $entry selection range 0 end
                        $entry icursor end
                    } else {
                        $self SetPathSilently $path
                        set selectFile $file
                        $self Done
                    }
                }
                PATH {
                    tk_messageBox -icon warning -type ok -parent $win \
                          -message "[format {Directory %s  does not exist.} $path]"
                    $entry selection range 0 end
                    $entry icursor end
                }
                CHDIR {
                    tk_messageBox -icon warning -type ok -parent $win \
                          -message [format "Cannot change to the directory %s.\nPermission denied." $path]
                    $entry selection range 0 end
                    $entry icursor end
                }
                ERROR {
                    tk_messageBox -icon warning -type ok -parent $win \
                          -message "[format {Invalid file name %s.} $path]"
                    $entry selection range 0 end
                    $entry icursor end
                }
            }
        }
        # method ResolveFile --
        #
        #	Interpret the user's text input in a file selection dialog.
        #	Performs:
        #
        #	(1) ~ substitution
        #	(2) resolve all instances of . and ..
        #	(3) check for non-existent files/directories
        #	(4) check for chdir permissions
        #
        # Arguments:
        #	context:  the current directory you are in
        #	text:	  the text entered by the user
        #	defaultext: the default extension to add to files with no extension
        #
        # Return vaue:
        #	[list $flag $directory $file]
        #
        #	 flag = OK	: valid input
        #	      = PATTERN	: valid directory/pattern
        #	      = PATH	: the directory does not exist
        #	      = FILE	: the directory exists by the file doesn't
        #			  exist
        #	      = CHDIR	: Cannot change to the directory
        #	      = ERROR	: Invalid entry
        #
        #	 directory      : valid only if flag = OK or PATTERN or FILE
        #	 file           : valid only if flag = OK or PATTERN
        #
        #	directory may not be the same as context, because text may contain
        #	a subdirectory name
        #
        method ResolveFile {context text defaultext} {
            set appPWD [pwd]
            set path [::tk::dialog::file::JoinFile $context $text]
            # If the file has no extension, append the default.  Be careful not
            # to do this for directories, otherwise typing a dirname in the box
            # will give back "dirname.extension" instead of trying to change dir.
            if {![file isdirectory [file join $options(-root) $path]] && 
                [string equal [file ext $path] ""]} {
                set path "$path$defaultext"
            }
            

            if {[catch {file exists [file join $options(-root) $path]}]} {
                # This "if" block can be safely removed if the following code
                # stop generating errors.
                #
                #	file exists ~nonsuchuser
                #
                return [list ERROR $path ""]
            }
            
            if {[file exists [file join $options(-root) $path]]} {
                if {[file isdirectory [file join $options(-root) $path]]} {
                    if {[catch {cd [file join $options(-root) $path]}]} {
                        return [list CHDIR $path ""]
                    }
                    set directory [pwd]
                    set file ""
                    set flag OK
                    cd $appPWD
                } else {
                    if {[catch {cd [file dirname [file join $options(-root) $path]]}]} {
                        return [list CHDIR [file dirname $path] ""]
                    }
                    set directory [pwd]
                    set file [file tail $path]
                    set flag OK
                    cd $appPWD
                }
            } else {
                set dirname [file dirname $path]
                if {[file exists [file join $options(-root) $dirname]]} {
                    if {[catch {cd [file join $options(-root) $dirname]}]} {
                        return [list CHDIR $dirname ""]
                    }
                    set directory [pwd]
                    set file [file tail $path]
                    if {[regexp {[*]|[?]} $file]} {
                        set flag PATTERN
                    } else {
                        set flag FILE
                    }
                    cd $appPWD
                } else {
                    set directory $dirname
                    set file [file tail $path]
                    set flag PATH
                }
            }
            
            set l [string length [file normalize $options(-root)]]
            incr l
            set directory [string range [file normalize $directory] $l end]
            
            return [list $flag $directory $file]
        }
        method SetPathSilently {path} {
            trace vdelete selectPath w [mymethod SetPath]
            set selectPath $path
            trace variable selectPath w [mymethod SetPath]
        }
        method SetPath {name1 name2 op} {
            #puts stderr "*** $self SetPath $name1 $name2 $op"
            #puts stderr "*** $self SetPath: winfo exists $win = [winfo exists $win]"
            if {[winfo exists $win]} {
                $self UpdateWhenIdle
            }      
        }
        method SetFilter {ftype} {
            set filter [lindex $ftype 1]
            $typeMenuBtn configure -text [lindex $ftype 0] -indicatoron 1
            if {![info exists extUsed]} {
                if {[string length $options(-defaultextension)]} {
                    set extUsed yes
                } else {
                    set extUsed no
                }
            }
            if {!$extUsed} {
                # Get the first extension in the list that matches {^\*\.\w+$}
                # and remove all * from the filter.
                set index [lsearch -regexp $filter {^\*\.\w+$}]
                if {$index >= 0} {
                    set options(-defaultextension) \
                          [string trimleft [lindex $filter $index] "*"]
                } else {
                    # Couldn't find anything!  Reset to a safe default...
                    set options(-defaultextension) ""
                }
            }
            $iconList see 0
            $self UpdateWhenIdle
        }
        method Update {} {
            #puts stderr "*** $self Update (enter)"
            if {![winfo exists $win]} {return}
            catch {unset updateId}
            set entCursor [$entry cget -cursor]
            set dlgCursor [$win       cget -cursor]
            $entry configure -cursor watch
            $win       configure -cursor watch
            update idletasks
            
            #puts stderr "*** $self Update: selectPath = $selectPath"
            
            
            $iconList deleteall
            set dirs [lsort -dictionary -unique \
                      [glob -tails \
                       -directory [file join $options(-root) \
                                   $selectPath] -type d \
                       -nocomplain *]]
            
            #puts stderr "[list *** $self Update: dirs = $dirs]"
            set dirList {}
            foreach d $dirs {
                lappend dirList $d
            }
            $iconList add $folderImage $dirList
            
            set directory [file join $options(-root) $selectPath]
            set directory [regsub {/.$} $directory {}]
            #puts stderr "*** $self Update: directory is $directory"
            set cmd [list glob -tails -directory $directory \
                                                  -type {f b c l p s} -nocomplain]
            #puts stderr "*** $self Update: filter = $filter"
            if {[string equal $filter *]} {
                lappend cmd .* *
            } else {
                lappend cmd {*}$filter
            }
            #puts stderr "*** $self Update: cmd = $cmd"
            set rawFiles [eval $cmd]
            #puts stderr [list *** $self Update: rawFiles = $rawFiles]
            set fileList [lsort -dictionary -unique $rawFiles]
            #puts stderr [list *** $self Update: fileList = $fileList]
            $iconList add $fileImage $fileList
            
            set list ""
            set dir ""
            #puts stderr "*** $self Update: file split selectPath = [file split $selectPath]"
            foreach subdir [file split $selectPath] {
                set dir [file join $dir $subdir]
                lappend list $dir
            }
            #puts stderr "[list *** $self Update: list = $list]"
            $dirmenu delete 0 end
            set var [myvar selectPath]
            foreach path $list {
                $dirmenu add command -label $path -command [list set $var $path]
            }
            switch $options(-saveoropen) {
                open {
                    $okButton configure -text Open -underline 0
                }
                save {
                    $okButton configure -text Save -underline 0
                }
            }
            
            $entry configure -cursor $entCursor
            $win       configure -cursor $dlgCursor
            #puts stderr "*** $self Update (exit)"
        }
        method Done {{_selectFilePath ""}} {
            if {[string equal $_selectFilePath ""]} {
                set _selectFilePath [::tk::dialog::file::JoinFile \
                                     $selectPath $selectFile]
            }
            if {"$options(-saveoropen)" eq "save"} {
                #	puts stderr "*** $self Done _selectFilePath = $_selectFilePath"
                if {[file exists [file join $options(-root) $_selectFilePath]]} {
                    set reply [tk_messageBox -icon warning -type yesno\
                               -parent $win -message \
                               [format "File %s already exists.\nDo you want to overwrite it?" $_selectFilePath]]
                    if {[string equal $reply "no"]} {return}
                }
            }
            set selectFilePath $_selectFilePath
        }
    }
}
    
## @}

package provide RPGUtilities 1.0


