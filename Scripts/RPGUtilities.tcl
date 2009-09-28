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
    method bindcanvas {args} {
      return [eval [list ::bind $canvas] $args]
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
    delegate option -initialdir to fileentry
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
		  lappend widgetopts -range {0 99 1}
		} else {
		  set range $attrlist_array(range)
		  if {[llength $range] > 1 && [llength $range] < 4} {
		    lappend widgetopts -range $range -text 0
		  } else {
		    lappend widgetopts -range {-9999999 9999999 1} -text 0
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
      ::RolePlayingDB3::PrintDialog printprogress setpageno $pageno
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
	    for {set i 0} {$i < $lines} {incr i} {$pdfobj newLine}
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
    pragma -hastypedestroy no
    pragma -hasinstances no
    pragma -hastypeinfo no

    typecomponent _printProgressDialog
    typecomponent   printprogressPageNoLE
    typeconstructor {
      set printerIcon [image create photo \
			-file [file join $::RolePlayingDB3::ImageDir \
					 largePrinter.gif]]
      set _printdialog {}
      set _printProgressDialog {}
    }
    ::RolePlayingDB3::GeneratePrintDialog {} {}
    typemethod create_printProgressDialog {} {
      if {"$_printProgressDialog" ne "" &&
	  [winfo exists "$_printProgressDialog"]} {return}
      set _printProgressDialog [Dialog .printProgressDialog \
					-image $printerIcon -default 0 \
					-modal none -parent . -side bottom \
					-title "Print Progress" -transient yes]
      $_printProgressDialog add -name dismis -text {Dismis} -state disabled \
      			        -command [mytypemethod _DismisPrintProgress]
      set frame [$_printProgressDialog getframe]
      set printprogressPageNoLE [LabelEntry $frame.printprogressPageNoLE \
				    -label {Printing Page:} \
				    -text "" -editable no]
      pack $printprogressPageNoLE -fill x
    }
    typemethod {printprogress start} {args} {
      set parent [from args -parent .]
      $type create_printProgressDialog
      $printprogressPageNoLE configure -text ""
      $_printProgressDialog configure -parent $parent
      wm transient [winfo toplevel $_printProgressDialog] $parent
      ::BWidget::focus set $printprogressPageNoLE
      ::BWidget::grab set  $_printProgressDialog
      $_printProgressDialog itemconfigure 0 -state disabled
      $_printProgressDialog draw
    }
    typemethod {printprogress setpageno} {pageno} {
      $printprogressPageNoLE configure -text "$pageno"
      update idle
    }
      
    typemethod {printprogress end} {} {
      $_printProgressDialog itemconfigure 0 -state normal
      ::BWidget::grab release  $_printProgressDialog
      ::BWidget::focus release $printprogressPageNoLE
    }
    typemethod _DismisPrintProgress {} {
      $_printProgressDialog withdraw
    }
  }
  snit::widget chroot_chooseDirectory {
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
      install banner using Label $win.banner -anchor w
      pack $banner -fill x
      install topframe using frame $win.topframe
      install dirlabel using Label $topframe.dirlabel -text "Directory:" \
						      -underline 0
      set dirmenubtn $topframe.dirmenu
      bind $dirlabel <<AltUnderlined>> [list focus $dirmenubtn]
      install dirmenu using tk_optionMenu $topframe.dirmenu \
						[myvar selectPath] ""
      install dirupbutton using Button $topframe.dirupbutton -image $updirImage
      $dirmenubtn configure -takefocus 1 -highlightthickness 2
      pack $dirupbutton -side right -padx 4 -fill both
      pack $dirlabel -side left -padx 4 -fill both
      pack $dirmenubtn -expand yes -fill both -padx 4
      install iconList using ::tk::IconList $win.iconList \
				-command [mymethod DblClick] -multiple no
      bind $iconList <<ListboxSelect>> [mymethod ListBrowse]
      install bottomframe using frame $win.bottomframe -bd 0
      install caption using Label $bottomframe.caption -text "Selection:" \
						       -underline 0 -anchor e \
						       -pady 0
      bind $caption <<AltUnderlined>> [list focus $bottomframe.entry]
      install entry using Entry $bottomframe.entry
      set iconlistData "::tk::$iconList"
      append iconlistData (font)
      set $iconlistData [$entry cget -font]
      install okButton using Button $bottomframe.okButton -text "OK" \
							  -underline 0 \
							  -default active \
							  -pady 3
      bind $okButton <<AltUnderlined>> [list $okButton invoke]
      install cancelButton  using Button $bottomframe.cancelButton \
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
      set selection  [tk::IconList_Curselection $iconList]
      if { [llength $selection] != 0 } {
	set filenameFragment \
	    [tk::IconList_Get $iconList [lindex $selection 0]]
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
      set items [::tk::IconList_Curselection $iconList]
      if {[llength $items] < 1} {return}
      set text [::tk::IconList_Get $iconList [lindex $items 0]]
      set file [::tk::dialog::file::JoinFile $selectPath $text]
      $entry delete 0 end
      $entry insert 0 $file
    }
    method CancelCmd {} {
      set selectFilePath ""
    }
    method UpDirCmd {} {
      if {"$selectPath" ne ""} {
#	puts stderr "[list *** $self UpDirCmd: selectPath = $selectPath]"
#	puts stderr "[list *** $self UpDirCmd: file dirname $selectPath = [file dirname $selectPath]]"
        set selectPath [file dirname $selectPath]
      }
    }
    method OkCmd {} {
      set selection [tk::IconList_Curselection $iconList]
      if { [llength $selection] != 0 } {
	set iconText [tk::IconList_Get $iconList [lindex $selection 0]]
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

#      puts stderr "*** $self Update: selectPath = $selectPath"

      ::tk::IconList_DeleteAll $iconList
      set dirs [lsort -dictionary -unique \
			[glob -tails \
			      -directory [file join $options(-root) \
						    $selectPath] -type d \
				-nocomplain *]]

#      puts stderr "[list *** $self Update: dirs = $dirs]"
      set dirList {}
      foreach d $dirs {
	lappend dirList $d
      }
      ::tk::IconList_Add $iconList $folderImage $dirList
      ::tk::IconList_Arrange $iconList
      set list "."
      set dir ""
#      puts stderr "*** $self Update: file split selectPath = [file split $selectPath]"
      foreach subdir [file split $selectPath] {
	set dir [file join $dir $subdir]
	lappend list $dir
      }
#      puts stderr "[list *** $self Update: list = $list]"
      $dirmenu delete 0 end
      set var [myvar selectPath]
      foreach path $list {
	$dirmenu add command -label $path -command [list set $var $path]
      }
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
      install banner using Label $win.banner -anchor w
      pack $banner -fill x
      install topframe using frame $win.topframe
      install dirlabel using Label $topframe.dirlabel -text "Directory:" \
						      -underline 0
      set dirmenubtn $topframe.dirmenu
      bind $dirlabel <<AltUnderlined>> [list focus $dirmenubtn]
      install dirmenu using tk_optionMenu $topframe.dirmenu \
						[myvar selectPath] ""
      install dirupbutton using Button $topframe.dirupbutton -image $updirImage
      $dirmenubtn configure -takefocus 1 -highlightthickness 2
      pack $dirupbutton -side right -padx 4 -fill both
      pack $dirlabel -side left -padx 4 -fill both
      pack $dirmenubtn -expand yes -fill both -padx 4
      install iconList using ::tk::IconList $win.iconList \
				-command [mymethod OkCmd] -multiple no
      bind $iconList <<ListboxSelect>> [mymethod ListBrowse]
      install bottomframe using frame $win.bottomframe -bd 0
      install caption using Label $bottomframe.caption -text "File name:" \
						       -underline 5 -anchor e \
						       -pady 0
      bind $caption <<AltUnderlined>> [list focus $bottomframe.entry]
      install entry using Entry $bottomframe.entry
      set iconlistData "::tk::$iconList"
      append iconlistData (font)
      set $iconlistData [$entry cget -font]
      install typeMenuLab using Button $bottomframe.typeMenuLab \
		-text "Files of type:" -underline 9 -anchor e  \
		-bd [$caption cget -bd] -relief [$caption cget -relief] \
		-padx [$caption cget  -padx] -pady [$caption cget  -pady]
      bindtags $typeMenuLab [list $typeMenuLab Label \
		[winfo toplevel $typeMenuLab] all]
      install typeMenuBtn using menubutton $bottomframe.typeMenuBtn \
				-indicatoron 1 -menu $bottomframe.typeMenuBtn.m
      install typeMenu using menu $typeMenuBtn.m -tearoff 0
      $typeMenuBtn configure -takefocus 1 -highlightthickness 2 \
		-relief raised -bd 2 -anchor w
      bind $typeMenuLab <<AltUnderlined>> [list focus $typeMenuBtn]
      install okButton using Button $bottomframe.okButton -text "OK" \
							  -underline 0 \
							  -default active \
							  -pady 3
      bind $okButton <<AltUnderlined>> [list $okButton invoke]
      install cancelButton  using Button $bottomframe.cancelButton \
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
#      puts stderr "*** method draw (after vwait): selectFilePath = $selectFilePath"
#      puts stderr "*** method draw (after vwait): file join $options(-root) $selectFilePath = [file join $options(-root) $selectFilePath]"
      if {"$selectFilePath" eq ""} {
	return $selectFilePath
      } else {
	return [file join $options(-root) $selectFilePath]
      }
    }
    method UpdateWhenIdle {} {
#      puts stderr "*** $self UpdateWhenIdle"
      if {![info exists updateId]} {
	set updateId [after idle [mymethod Update]]
#	puts stderr "*** $self UpdateWhenIdle: new updateId = $updateId"
      } else {
#	puts stderr "*** $self UpdateWhenIdle: old updateId = $updateId"
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
	  tk_messageBox -type ok -parent $win  -message \
	  	[format "Cannot change to the directory %s.\nPermission denied." $file]] \
	  	-icon warning
	} else {
	  cd $appPWD
#	  puts stderr "*** $self ListInvoke: setting selectPath to $file"
	  set selectPath $file
        }
      } else {
#        puts stderr "*** $self ListInvoke: setting selectFile to $file"
        set selectFile $file
        $self Done
      }
    }
    method ListBrowse {} {
      set items [::tk::IconList_Curselection $iconList]
      if {[llength $items] < 1} {return}
      set text [::tk::IconList_Get $iconList [lindex $items 0]]
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
#	puts stderr "[list *** $self UpDirCmd: selectPath = $selectPath]"
#	puts stderr "[list *** $self UpDirCmd: file dirname $selectPath = [file dirname $selectPath]]"
        set selectPath [file dirname $selectPath]
      }
    }
    method OkCmd {} {
#      puts stderr "*** $self OkCmd"
      set filenames {}
      foreach item [::tk::IconList_Curselection $iconList] {
	lappend filenames [::tk::IconList_Get $iconList $item]
      }
#      puts stderr "[list *** $self OkCmd: filenames = $filenames]"
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
#      puts stderr "*** $self SetPath $name1 $name2 $op"
#      puts stderr "*** $self SetPath: winfo exists $win = [winfo exists $win]"
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
      set iconlistData "::tk::$iconList"
      append iconlistData (sbar)
      [set $iconlistData] set 0.0 0.0
      $self UpdateWhenIdle
    }
    method Update {} {
      if {![winfo exists $win]} {return}
      catch {unset updateId}
      set entCursor [$entry cget -cursor]
      set dlgCursor [$win       cget -cursor]
      $entry configure -cursor watch
      $win       configure -cursor watch
      update idletasks

#      puts stderr "*** $self Update: selectPath = $selectPath"

      ::tk::IconList_DeleteAll $iconList
      set dirs [lsort -dictionary -unique \
			[glob -tails \
			      -directory [file join $options(-root) \
						    $selectPath] -type d \
				-nocomplain *]]

#      puts stderr "[list *** $self Update: dirs = $dirs]"
      set dirList {}
      foreach d $dirs {
	lappend dirList $d
      }
      ::tk::IconList_Add $iconList $folderImage $dirList

      set cmd [list glob -tails -directory [file join $options(-root) \
						    $selectPath] \
			 -type {f b c l p s} -nocomplain]
#      puts stderr "$self Update: filter = $filter"
      if {[string equal $filter *]} {
	lappend cmd .* *
      } else {
	eval [list lappend cmd] *$filter
      }
#      puts stderr "$self Update: cmd = $cmd"
      set fileList [lsort -dictionary -unique [eval $cmd]]
#      puts stderr "$self Update: fileList = $fileList"
      ::tk::IconList_Add $iconList $fileImage $fileList

      ::tk::IconList_Arrange $iconList

      set list "."
      set dir ""
#      puts stderr "*** $self Update: file split selectPath = [file split $selectPath]"
      foreach subdir [file split $selectPath] {
	set dir [file join $dir $subdir]
	lappend list $dir
      }
#      puts stderr "[list *** $self Update: list = $list]"
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



package provide RPGUtilities 1.0


	      

