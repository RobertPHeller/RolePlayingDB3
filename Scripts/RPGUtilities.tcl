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

package require BWidget
package require snit
package require xml
package require BWLabelComboBox
package require BWLabelSpinBox
package require BWFileEntry
package require LabelSelectColor
package require pdf4tcl

namespace eval RolePlayingDB3 {
  snit::widgetadaptor LabeledDirTree {
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
    method validdirectory {option value} {
#

      #if {"$value" eq ""} {return};# Special non-value
      if {[file isdirectory "$value"] && [file readable "$value"]} {
	return $value
      } else {
	error "Expected a readable directory for $option, got $value"
      }
    }
    method newdirectory {option value} {
      set options($option) "$value"
      $self redrawdirtree
    }
    option {-sortfunction sortFunction SortFunction} -default {} \
					-configuremethod newdirectory
    option {-opendirs openDirs OpenDirs} -default yes -type snit::boolean \
					-configuremethod newdirectory
    option {-nofiles  noFiles NoFiles} -default no -type snit::boolean \
					-configuremethod newdirectory
    typevariable openfold
    typeconstructor {
      set openfold [image create photo -file [file join $::BWIDGET::LIBRARY \
							images openfold.gif]]
    }
    component scroll
    component tree
    delegate option -label to hull as -text
    delegate method * to tree except {cget configure xview yview insert delete
				      move reorder itemconfigure itemcget 
				      parent}
    delegate option * to tree except {-xscrollcommand -yscrollcommand}
    method itemcget {node option} {
      switch -glob -- $option {
	-fullpath {return [$tree itemcget $node -data]}
	-dirnode  {return [$tree parent   $node]}
	default {
		error "Unsuported option $option, should be one of -fullpath or -dirnode"
	}
      }
    }
    proc dirtree {tree parent dir pattern showextension sortfunction opendirs nofiles} {
#      puts stderr "*** [namespace which dirtree] $tree $parent $dir $pattern $showextension"
      set unsorteddirs [glob -nocomplain -type d [file join $dir *]]
      if {"$sortfunction" eq ""} {
	set sorteddirs [lsort -dictionary $unsorteddirs]
      } else {
	set sorteddirs [lsort -command $sortfunction $unsorteddirs]
      }
      foreach directory $sorteddirs {
	regsub -all {[[:space:]]} "[file rootname [file tail $directory]]" {_} nodename
#	puts stderr "*** [namespace which dirtree]: directory = $directory, nodename = $nodename"
	set thisnode [$tree insert end "$parent" \
			"$nodename#auto" \
			-data "$directory" \
			-text "[file tail $directory]" \
			-open $opendirs \
			-image "$openfold"]
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
#	puts stderr "*** [namespace which dirtree]: file = $file, nodename = $nodename"
	if {$showextension} {
	  set text [file tail $file]
	} else {
	  set text [file rootname [file tail $file]]
	}
	$tree insert end $parent \
			"$nodename#auto" \
			-data $file \
			-text "$text" \
			-open no
      }
    }
    method redrawdirtree {} {
      if {"$options(-directory)" eq ""} {return}
      $tree delete [$tree nodes root]
      dirtree $tree root $options(-directory) $options(-filepattern) \
			 $options(-showextension) $options(-sortfunction) \
			 $options(-opendirs) $options(-nofiles)
    }
    constructor {args} {
      set options(-isnewobject) [from args -isnewobject]
      set options(-auto) [from args -auto]
      set options(-scrollbar) [from args -scrollbar]
      installhull using TitleFrame
      install scroll using ScrolledWindow [$hull getframe].scroll \
					-auto $options(-auto) \
					-scrollbar $options(-scrollbar)
      pack $scroll -fill both -expand yes
      install tree using Tree [$scroll getframe].tree
      pack $tree -fill both -expand yes
      $scroll setwidget $tree
      $self configurelist $args
    }
  }
  snit::widget ScrolledCanvas {
    component scroll
    component canvas
    delegate option * to canvas except {-xscrollcommand -yscrollcommand}
    delegate method * to canvas except {cget configure xview yview}
    option  -auto -readonly yes -default both \
		  -type {snit::enum -values {none both vertical horizontal}}
    option  -scrollbar -readonly yes -default both \
		  -type {snit::enum -values {none both vertical horizontal}}
    constructor {args} {
      set auto [from args -auto]
      set scrollbar [from args -scrollbar]
      install scroll using ScrolledWindow $win.scroll \
					-auto $auto -scrollbar $scrollbar
      pack $scroll -fill both -expand yes
      install canvas using canvas [$scroll getframe].canvas
      pack $canvas -fill both -expand yes
      $scroll setwidget $canvas
      $self configurelist  $args
    }
  }
  snit::widget ScrolledList {
    component scroll
    component list
    delegate option * to list except {-xscrollcommand -yscrollcommand}
    delegate method * to list except {cget configure xview yview}
    option  -auto -readonly yes -default both \
		  -type {snit::enum -values {none both vertical horizontal}}
    option  -scrollbar -readonly yes -default both \
		  -type {snit::enum -values {none both vertical horizontal}}
    constructor {args} {
      set auto [from args -auto]
      set scrollbar [from args -scrollbar]
      install scroll using ScrolledWindow $win.scroll \
					-auto $auto -scrollbar $scrollbar
      pack $scroll -fill both -expand yes
      install list using ListBox [$scroll getframe].list \
				-selectmode [from args -selectmode none]
      pack $list -fill both -expand yes
      $scroll setwidget $list
      $self configurelist  $args
    }
  }
  snit::widget Graphic {
    component label
    component fileentry
    option -modifycmd -default {}
    option -text -default {} -cgetmethod gettext -configuremethod settext
    option {-basedirectory baseDirectory BaseDirectory} -default {}
    delegate option -label to fileentry
    delegate option -labelwidth to fileentry
    method gettext {option} {return [$fileentry cget $option]}
    method settext {option value} {
      $fileentry configure $option $value
      $self updatepicture
    }
    method getimage {} {return [$label cget -image]}
    constructor {args} {
      install label using Label $win.label
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
      set fullpath [$fileentry cget -text]
#      puts stderr "*** $self updatepicture: fullpath (from FE) is $fullpath"
#      puts stderr "*** $self updatepicture: options(-basedirectory) is $options(-basedirectory)"
#      puts stderr "*** $self updatepicture: pathtype of $fullpath is [file pathtype $fullpath]"
      if {[file pathtype $fullpath] eq "relative"} {
	set fullpath [file join $options(-basedirectory) $fullpath]
      }
#      puts stderr "*** $self updatepicture: fullpath (final) is $fullpath"
      if {[catch {image create photo -file $fullpath} newimage]} {
	puts stderr "*** $self updatepicture: image create photo failed: $newimage"
	return
      }
#      puts stderr "*** $self updatepicture: newimage is $newimage"
      set oldimg [$label cget -image]
      if {"$oldimg" ne ""} {image delete $oldimg}
      $label configure -image $newimage
    }
    method filemodified {} {
      $self updatepicture
      if {"$options(-modifycmd)" ne ""} {
	uplevel \#0 "$options(-modifycmd)"
      }
    }
  }
  snit::widgetadaptor LabeledScrolledText {
    component scroll
    component text
    delegate option * to text except {-xscrollcommand -yscrollcommand}
    delegate method * to text except {cget configure xview yview}
    delegate option -label to hull as -text
#    delegate option -labelfont to hull as -font
#    delegate option -labelside to hull as -side
#    delegate option -labelbaseline to hull as -baseline
    option  -auto -readonly yes -default both \
		  -type {snit::enum -values {none both vertical horizontal}}
    option  -scrollbar -readonly yes -default both \
		  -type {snit::enum -values {none both vertical horizontal}}
    option -text -cgetmethod gettext -configuremethod puttext
    method gettext {option} {
      return "[$text get 1.0 end-1c]"
    }
    method puttext {option value} {
      $text delete 1.0 end
      $text insert end "$value"
    }
    method bind {args} {
      eval [list ::bind $text] $args
    }
    constructor {args} {
      set auto [from args -auto]
      set scrollbar [from args -scrollbar]
      installhull using TitleFrame
      install scroll using ScrolledWindow [$hull getframe].scroll \
					-auto $auto -scrollbar $scrollbar
      pack $scroll -fill both -expand yes
      install text using text [$scroll getframe].text
      pack $text -fill both -expand yes
      $scroll setwidget $text
      $self configurelist  $args
    }
  }
  snit::macro ::RolePlayingDB3::OpenFilename {{optionspec {-openfilename openFilename OpenFilename}}} {
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
    typevariable _typenodeStack
    typevariable _typenodeTree -array {}
    typevariable _typenodeInfo -array {}
    typevariable _typesearchmatch
    typevariable _founddata
    typevariable _elementIndex
    typemethod _typeelementstart {tag attrlist args} {
#      puts stderr "*** $type _typeelementstart $tag $attrlist $args"
      set parent [lindex $_typenodeStack end]
      lappend _typenodeStack $_elementIndex
#      puts stderr "*** $type _typeelementstart: _typenodeStack = $_typenodeStack"
      set _typenodeInfo($_elementIndex) [list $tag $attrlist $args]
      lappend _typenodeTree($parent) $_elementIndex
      set _typenodeTree($_elementIndex) {}
#      puts stderr "*** $type _typeelementstart: _typenodeTree($_elementIndex) = $_typenodeTree($_elementIndex)"
      incr _elementIndex
    }
    typemethod _typeelementend {tag args} {
#      puts stderr "*** $type _typeelementend $tag $args"
      set _typenodeStack [lrange $_typenodeStack 0 [expr {[llength $_typenodeStack] -2}]]
#      puts stderr "*** $type _typeelementend: _typenodeStack = $_typenodeStack"
    }
    typemethod _typecharacterdata {data} {
#      puts stderr "*** $type _typecharacterdata: _typenodeStack = $_typenodeStack"
      set curindex [lindex $_typenodeStack end]
#      puts stderr "*** $type _typeelementstart: _typenodeTree($curindex) = $_typenodeTree($curindex)"
      foreach {tag attrlist args} $_typenodeInfo($curindex) {break}
      set cmd $_typesearchmatch
      lappend cmd "$tag" "$attrlist" "$args"
      if {[uplevel \#0 "$cmd"]} {
	set _founddata [string trim $data]
      }
    }
    typemethod ExtractTagValue {XML matchscript default} {
      set _typesearchmatch $matchscript
      set _founddata $default
      set p [xml::parser -elementstartcommand [mytypemethod _typeelementstart] \
			 -elementendcommand   [mytypemethod _typeelementend] \
			 -characterdatacommand [mytypemethod _typecharacterdata]]
      set _typenodeStack {root}
      array unset _typenodeTree
      array unset _typenodeInfo
      set _typenodeInfo(root) {root {x} {x}}
      set _typenodeTree(root) {}
      set _elementIndex 0
      $p parse $XML
      $p free
      return $_founddata
    }
    typemethod ContainerTree {XML} {
      set p [xml::parser -elementstartcommand [mytypemethod _typeelementstart] \
			 -elementendcommand   [mytypemethod _typeelementend]]
      set _typenodeStack {root}
      array unset _typenodeTree
      array unset _typenodeInfo
      set _typenodeInfo(root) {root {x} {x}}
      set _typenodeTree(root) {}
      set _elementIndex 0
      $p parse $XML
      $p free
      return [makeContainerTree root]
    }
    proc makeContainerTree {node} {
      if {[catch {set _typenodeInfo($node)} data]} {return [list]}
      if {[catch {set _typenodeTree($node)} children]} {return [list]}
      foreach {tag attrlist args} $data {break}
      if {"$node" eq "root"} {
	if {[llength $children] == 1} {
	  return [makeContainerTree [lindex $children 0]]
	} elseif {[llength $children] == 0} {
	  return [list]
	} else {
	  set result [list]
	}	    
      } else {
        if {"$tag" eq "Field"} {
	  set nameIndx [lsearch $attrlist name]
	  if {$nameIndx < 0} {
	    set name {unknown}
	  } else {
	    incr nameIndx
	    set name [lindex $attrlist $nameIndx]
	  }
	  set result "$tag:$name"
	} else {
	  set result $tag
	}
      }
      if {[llength $children] == 0} {return $result}
      set result [list $result]
      foreach n $children {
        set subTree [makeContainerTree $n]
	if {[llength $subTree] > 0} {lappend result $subTree}
      }
      return $result
    }
    option -xml -readonly yes -default {}
    option {-templatevariable templateVariable TemplateVariable} -default {} \
								 -readonly yes
    option {-dirtyvariable dirtyVariable DirtyVariable} -default {}
    method setdirty {} {
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
    variable nodeStack
    variable nodeTree -array {}
    variable fileWidgets -array {}
    variable generatedTemplate {}
    variable needNamelist yes
    variable idmap -array {}
    method getElementWidgetById {id} {
      if {[catch {set idmap($id)} w]} {
	return {}
      } else {
	return $w
      }
    }
    variable pageno 0
    method getpageno {} {return $pageno}
    variable lineno 1000
    method getlineno {} {return $lineno}
    method bcmdmethod {{id ""}} {
      if {$options(-buttoncommand) ne ""} {
	uplevel \#0 "$options(-buttoncommand) $id"
      }
    }
    component editframe
    constructor {args} {
      set options(-xml) [from args -xml]
      if {"$options(-xml)" eq ""} {
	error "The -xml option is a required option!"
      }
      installhull using ScrolledWindow -scrollbar vertical -auto vertical
      install editframe using ScrollableFrame [$hull getframe].editframe \
			-constrainedwidth yes
      pack $editframe -fill both -expand yes
      $hull setwidget $editframe
      $self configurelist $args
      
      set p [xml::parser -elementstartcommand [mymethod _elementstart] \
			 -elementendcommand   [mymethod _elementend] \
			 -characterdatacommand [mymethod _characterdata]]
      set nodeStack [list $editframe]
      array unset nodeTree
      if {"$options(-templatevariable)" ne ""} {
	set generatedTemplate {<?xml version="1.0" ?>}
	append generatedTemplate "\n"
      }
      $p parse $options(-xml)
      $p free
      if {"$options(-templatevariable)" ne ""} {
	upvar #0 "$options(-templatevariable)" template
	set template $generatedTemplate
      }
      if {"$options(-xmlfile)" ne ""} {$self recreateXML "$options(-xmlfile)"}
    }
    proc quoteXML {text} {
      regsub -all {&} $text   {\&amp;} quoted
      regsub -all {<} $quoted {\&lt;} quoted
      regsub -all {>} $quoted {\&gt;} quoted
      regsub -all {'} $quoted {\&apos;} quoted
      regsub -all {"} $quoted {\&quot;} quoted
      return "$quoted"
    }
    proc makeattrlist {attrlist} {
      set result {}
      foreach {n v} $attrlist {
	append result "$n=\"[quoteXML $v]\" "
      }
      return $result
    }    
    method makewidget {tag attrlist arglist} {
#      puts stderr "*** [list $self makewidget $tag $attrlist $arglist]"
      set nodename $tag
      set widget none
      set bindscript {}
      set label  {}
      set widgetopts [list]
      set packopts [list]
      array unset attrlist_array
      array set attrlist_array $attrlist
      if {![catch {set attrlist_array(side)} side]} {
	lappend packopts -side $side
      }
      switch -exact [string totitle $tag] {
	Li {
	  set curnode [lindex $nodeStack end]
	  set index [$curnode insert end #auto -data $attrlist]
#	  puts stderr "*** $self makewidget: curnode = $curnode, inserted $attrlist at $index, $curnode index end is [$curnode index end]"
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
		} elseif {[regexp {^[dD]%$} "$attrlist_array(generator)"] > 0} {
		  lappend widgetopts -range {1 100 1}
		} else {
		  set range $attrlist_array(range)
		  if {[llength $range] > 1 && [llength $range] < 4} {
		    lappend widgetopts -range $range
		  }
		}
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
		set bindscript [list bind <KeyPress> [mymethod setdirty]]
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
				 -basedirectory $options(-basedirectory)
	    }
	    Document {
	      set widget FileEntry
	      set bindscript [list bind <KeyPress> [mymethod setdirty]]
	      lappend widgetopts -modifycmd [mymethod setdirty]
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
	    lappend widgetopts -selectmode single
	  } else {
	    lappend widgetopts -selectmode $selectmode
	  }
	  set widget ::RolePlayingDB3::ScrolledList
#	  puts stderr "*** $self makewidget: widget = $widget, widgetopts = $widgetopts"
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
	  set curnode [lindex $nodeStack end]
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
	    lappend widgetopts -name $name
	  }
	  if {[catch {set attrlist_array(id)} id]} {
	    lappend widgetopts -command [mymethod bcmdmethod "$label"]
	    set id {}
	  } else {
	    lappend widgetopts -command [mymethod bcmdmethod "$id"]
	  }
#	  puts stderr "*** $self makewidget: curnode = $curnode, tag = $tag, widgetopts = $widgetopts"
	  set w [eval [list $curnode add] $widgetopts]
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
	  set widget TitleFrame
	  lappend widgetopts -text $nodename
	}
      }
      regsub -all {[[:space:]]} $nodename {} wname
      set curroot [[lindex $nodeStack end] getframe]
      set wname $curroot.[string tolower $wname]
      set bwname $wname
      set i 0
      while {[winfo exists $wname]} {
	incr i
	set wname $bwname$i
      }
#      puts stderr "*** $self makewidget: wname = $wname, tag = $tag, widget = $widget, packopts = $packopts, widgetopts = $widgetopts"
      eval [list $widget $wname] $widgetopts
#      if {"$widget" eq "::RolePlayingDB3::ScrolledList"} {
#	puts stderr "*** $self makewidget: wname = $wname, $wname cget -selectmode = [$wname cget -selectmode]"
#      }
#      puts stderr "*** $self makewidget: winfo exists $wname = [winfo exists $wname]"
      if {"$widget" eq "FileEntry" || 
	  "$widget" eq "::RolePlayingDB3::Graphic"} {
	set fileWidgets($wname) true
      }
      if {"$widget" eq "LabelComboBox"} {$wname setvalue first}
      eval [list pack $wname] $packopts
      if {![catch {set attrlist_array(id)} id]} {
	set idmap($id) $wname
      }
      if {"$bindscript" ne ""} {eval $wname $bindscript}
      lappend nodeStack $wname
      set nodeTree($wname) [list $tag $attrlist $arglist]
    }
    
    method _elementstart {tag attrlist args} {
#      puts stderr "*** [list _elementstart $tag $attrlist $args]"
      set wname [$self makewidget $tag $attrlist $args]
      if {[string totitle $tag] eq "Li"} {return}
      if {"$options(-templatevariable)" ne ""} {
	set indent [string repeat {  } [llength $nodeStack]]
	append generatedTemplate "$indent<rpgv3:$tag "
	if {$needNamelist} {
	  append generatedTemplate "xmlns:rpgv3=\""
	  set namespace [lindex $args [expr {[lsearch -exact $args -namespace] + 1}]]
	  append generatedTemplate "[quoteXML $namespace]"
	  append generatedTemplate "\" "
	  set needNamelist no
	}	    
	append generatedTemplate "[makeattrlist $attrlist]"
	switch -exact [string totitle $tag] {
	  Bi -
	  Canvas -
	  Button -
	  Field {append generatedTemplate "/>\n"}
	  default {append generatedTemplate ">\n"}
	}
      }
    }
    method _elementend {tag args} {
#      puts stderr "*** [list _elementend $tag $args]"
      switch -exact [string totitle $tag] {
	Bi -
	Li {
	}
	Canvas -
	Button -
	Buttonbox -
	Field {
	  set nodeStack [lrange $nodeStack 0 [expr {[llength $nodeStack] - 2}]]
	}
	default {
	  if {"$options(-templatevariable)" ne ""} {
	    set indent [string repeat {  } [llength $nodeStack]]
	    append generatedTemplate "$indent</rpgv3:$tag >\n"
	  }
	  set nodeStack [lrange $nodeStack 0 [expr {[llength $nodeStack] - 2}]]
	}
      }
    }
    method _characterdata {data} {
#      puts stderr "*** $self _characterdata $data"
      set curnode [lindex $nodeStack end]
      set data [string trim "$data"]
#      puts stderr "*** $self _characterdata: curnode = $curnode"
#      puts stderr "*** $self _characterdata: data = '$data'"
      if {"$data" ne ""} {
#        puts stderr "*** $self _characterdata: winfo class $curnode is [winfo class $curnode]"
	if {[winfo class $curnode] eq "ScrolledList"} {
	  set index [lindex [$curnode items] end]
	  $curnode itemconfigure $index -text "$data"
	} else {
	  catch {$curnode configure -text "$data"}
	}
      }
      if {"$options(-templatevariable)" ne ""} {
	if {[winfo class $curnode] eq "TitleFrame"} {
	  append generatedTemplate "[quoteXML $data]\n"
	}
      }
    }
    method recreateXML {file} {
      if {[catch {open $file w} shfp]} {
	tk_messageBox -type ok -icon error -message "Internal error: cannot create $file: $shfp"
	return
      }
      puts $shfp {<?xml version="1.0" ?>}
      $self _recreateXML_processNodesAt $editframe $shfp
      close $shfp
    }
    method childrenofgetframe {w} {
      if {[catch {winfo children [$w getframe]} children]} {
	return {}
      } else {
	set result [list]
	foreach c $children {
	  if {[catch {set nodeTree($c)}]} {continue}
	  lappend result $c
	}
      }
      return $result
    }
    method _recreateXML_processNodesAt {node fp {needxmlns yes} {indent {}}} {
      foreach n [$self childrenofgetframe $node] {
	if {[catch {set nodeTree($n)} data]} {continue}
	foreach {tag attrlist args} $data {break}
	puts -nonewline $fp "$indent<rpgv3:$tag "
	if {$needxmlns} {
	  puts -nonewline $fp "xmlns:rpgv3=\""
	  set namespace [lindex $args [expr {[lsearch -exact $args -namespace] + 1}]]
	  puts -nonewline $fp "[quoteXML $namespace]"
	  puts -nonewline $fp "\" "
	  set needxmlns no
	}
	puts -nonewline $fp [makeattrlist $attrlist]
	switch -exact [string totitle "$tag"] {
	  Field {
	    puts -nonewline $fp ">"
	    if {[catch {set fileWidgets($n)} fileflag]} {
	      puts -nonewline $fp "[quoteXML [$n cget -text]]"
	    } else {
	      set curfile "[$n cget -text]"
	      set storedfile "$curfile"
	      if {"$curfile" ne "" && "$options(-filewidgethandler)" ne ""} {
		set storedfile [uplevel \#0 "$options(-filewidgethandler)" "$curfile"]
	      }
	      puts -nonewline $fp "[quoteXML $storedfile]"
	    }
	    puts $fp "$indent</rpgv3:$tag>"
	  }
	  List {
	    puts $fp ">"
	    foreach li [$n items] {
	      puts -nonewline $fp "$indent  <rpgv3:Li [makeattrlist [$n itemcget $li -data]] >"
	      puts $fp "[quoteXML [$n itemcget $li -text]]</rpgv3:Li>"
	    }
	    puts $fp "$indent</rpgv3:$tag>"
	  }
	  Buttonbox {
	    puts -nonewline $fp ">"
	    for {set i 0} {$i <= [$n index end]} {incr i} {
	      set alist [list]
	      foreach {opt attr} {-text label -name name} {
		set v [$n itemcget $i $opt]
		if {"$v" ne ""} {lappend alist $attr $v}
	      }
	      set cmd [$n itemcget $i -command]
	      set v [lindex $cmd end]
	      if {"$v" ne ""} {lappend alist id $v}
	      puts $fp "$indent  <rpgv3:Bi [makeattrlist $alist] />" 
	    }
	    puts $fp "$indent</rpgv3:$tag>"
	  }
	  Button -
	  Canvas {
	    puts $fp "/>"
	  }
	  default {
	    puts -nonewline $fp ">"
	    puts $fp "[quoteXML [$n cget -text]]"
	    $self _recreateXML_processNodesAt $n $fp $needxmlns "$indent  "
	    puts $fp "$indent</rpgv3:$tag>"
	  }
	}
      }
    }
    method outputXMLToPDF {pdfobj {heading "Sheet"} {curpage 0} {curline 1000}} {
      $pdfobj setFont 10 Courier
      $pdfobj setLineSpacing [expr {14.0 / 10.0}]
      set pageno $curpage
      set lineno $curline
      $self _outputXMLToPDF_processNodesAt $heading {} $editframe $pdfobj
    }
    method newPDFPage {pdfobj heading subheading} {
      incr pageno
      set lineno 1
      $pdfobj startPage
      set width [lindex [$pdfobj getDrawableArea] 0]
      $pdfobj text "$subheading" -x 0 -y 0
      $pdfobj text "$heading" -align center -x [expr {$width/2.0}] -y 0
      $pdfobj text "Page $pageno" -align right -x $width -y 0
      $pdfobj setTextPosition 0 [expr {$lineno * 14}]
    }
    method _outputXMLToPDF_processNodesAt {heading subheading node pdfobj {indent 0}} {
      foreach n [$self childrenofgetframe $node] {
	if {[catch {set nodeTree($n)} data]} {continue}
	foreach {tag attrlist args} $data {break}
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
		regsub {\.[[:digit:]]*$} [$n index end-1c] {} lines
		for {set i 1} {$i <= $lines} {incr i} {
		  if {($y + 14) >= $bottom} {
		    $self newPDFPage $pdfobj $heading "$subheading"
		    set y [expr {$lineno * 14}]
		  }
		  $pdfobj text [$n get ${i}.0 "${i}.0 lineend"] -x $indent
		  $pdfobj newLine
		  incr lineno
		  set y [expr {$lineno * 14}]
		}
	      }
	      Graphic {
		set image [$n getimage]
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
		set text  [$n cget -text]
#		puts stderr "*** $self _outputXMLToPDF_processNodesAt: pageno = $pageno, lineno = $lineno"
		if {$pageno < 1 || [expr {$lineno * 14}] > [lindex [$pdfobj getDrawableArea] 1]} {
		  $self newPDFPage $pdfobj $heading "$subheading"
		}
		$pdfobj text "$label: $text" -x $indent
		$pdfobj newLine
		incr lineno
#		puts stderr "*** $self _outputXMLToPDF_processNodesAt: lineno = $lineno (after Other field)"
	      }
	    }
	  }
	  List {
	    foreach li [$n items] {
	      set text [$n itemcget $li -text]
	      if {$pageno < 1 || [expr {$lineno * 14}] > [lindex [$pdfobj getDrawableArea] 1]} {
		$self newPDFPage $pdfobj "$heading" "$subheading"
	      }
	      $pdfobj text "$text" -x $indent
	      $pdfobj newLine
	      incr lineno
	    }
	  }
	  Canvas {
	    set dheight [expr {[lindex [$pdfobj getDrawableArea] 1] - ($lineno * 14)}]
	    set dwidth [expr {[lindex [$pdfobj getDrawableArea] 0] - $indent}]
	    set bbox [$n bbox all]
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
	    $pdfobj canvas $n -x $indent -y [expr {$lineno * 14}] \
			      -height $height -bg yes
	    set lines [expr {int(ceil(double($height) / 14.0))+2}]
	    $pdfobj newLine $lines
	    incr lineno $lines
	  }
	  Button -
	  Buttonbox {
	  }
	  default {
	    set subheading [$n cget -text]
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
	    $self _outputXMLToPDF_processNodesAt "$heading" "$subheading" $n $pdfobj [expr {$indent + 24}]
	  }
	}
      }
    }
  }
  snit::macro ::RolePlayingDB3::GeneratePrintDialog {additionalcomps createbody} {
    typecomponent _printdialog
    typecomponent   printfileFE
    typecomponent   papersizeLCB
    foreach c $additionalcomps {
      typecomponent $c
    }
    typevariable printerIcon
    typevariable printerfiletypes { {{PDF Files} {.pdf}       }
				    {{All Files} *            } }
    set createPrintDialogBody {
      if {"$_printdialog" ne "" && [winfo exists $_printdialog]} {return}
      set _printdialog [Dialog .printdialog -image $printerIcon \
				-cancel 1 -default 0 -modal local \
				-parent . -side bottom \
				-title "Print" -transient yes]
      $_printdialog add -name print  -text {Print}
      $_printdialog add -name cancel -text {Cancel}
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
      $papersizeLCB setvalue first
    }
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
	  set result [::pdf4tcl::new %AUTO% -paper [$papersizeLCB cget -text] \
					    -file  [$printfileFE  cget -text] \
					    -margin 36]
	}
	1 {
	  set result {}
	}
      }
      return $result
    }
  }

  snit::type PrintDialog {
    pragma -hastypedestroy no
    pragma -hasinstances no
    pragma -hastypeinfo no

    typeconstructor {
      set printerIcon [image create photo \
			-file [file join $::RolePlayingDB3::ImageDir \
					 largePrinter.gif]]
      set _printdialog {}
    }
    ::RolePlayingDB3::GeneratePrintDialog {} {}
  }
}



package provide RPGUtilities 1.0


	      

