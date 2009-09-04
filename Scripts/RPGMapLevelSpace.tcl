#* 
#* ------------------------------------------------------------------
#* Role PlayingDB V3.0 by Deepwoods Software
#* ------------------------------------------------------------------
#* RPGMapLevelSpace.tcl - Map, Level, Space objects
#* Created by Robert Heller on Wed Sep  2 12:45:33 2009
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

package require vfs::zip
package require vfs::mk4
package require ZipArchive
package require RPGUtilities

namespace eval RolePlayingDB3 {
  snit::macro ::RolePlayingDB3::MapBundleMountPoint {} {
    option {-mapbundlemountpoint mapBundleMountPoint MapBundleMountPoint} \
		-readonly yes -default /MAP -validatemethod validatemountpoint
    proc isvalidmountpoint {mp} {
      foreach {system additional} [file system $mp] {break}
      if {"$system" eq "tclvfs" &&
	  "$additional" eq "::vfs::mk4::handler mk4vfs1"} {
	return yes
      } else {
	return no
      }
    }
    method validatemountpoint {option value} {
      if {[file exists $value] && [file readable $value] &&
	  [file writable $value] && [file isdirectory $value] &&
	  [isvalidmountpoint $value]} {
	return $value
      } else {
	error "Expected a valid tclvfs / mk4vfs1 mount point for $option, but got $value"
      }
    }     
  }
  snit::macro ::RolePlayingDB3::MapEditorOption {} {
    option {-mapeditor mapEditor MapEditor} \
		-readonly yes -default {} -validatemethod validatemapeditor
    method validatemapeditor {option value} {
      if {[winfo exists $value] &&
	  [winfo toplevel $value] eq "$value" &&
	  [winfo class $value] eq "MapEditor"} {
	return $value
      } else {
	error "Expected a Map Editor for $option, but got $value"
      }
    }
  }
  snit::macro ::RolePlayingDB3::LevelEditorOption {} {
    option {-leveleditor levelEditor LevelEditor} \
		-readonly yes -default {} -validatemethod validatemapeditor
    method validatemapeditor {option value} {
      if {[winfo exists $value] &&
	  [winfo toplevel $value] eq "$value" &&
	  [winfo class $value] eq "LevelEditor"} {
	return $value
      } else {
	error "Expected a Level Editor for $option, but got $value"
      }
    }
  }
  snit::macro ::RolePlayingDB3::LevelDirOption {} {
    option {-leveldir levelDir LevelDir} \
		-readonly yes -default {} -validatemethod validateleveldir
    method validateleveldir {option value} {
      if {[file isdirectory $value] && [file writable $value] &&
	  [file tail [file dirname $value]] eq "Levels" &&
	  [isvalidmountpoint [file dirname [file dirname $value]]]} {
	return $value
      } else {
	error "Expected a valid level directory for $option, but got $value"
      }
    }	
  }
  snit::macro ::RolePlayingDB3::SpaceFileOption {} {
    option {-spacefile spaceFile SpaceFile} \
		-readonly yes -default {} -validatemethod validatespacefile
    method validatespacefile {option value} {
      if {![file isdirectory $value] && 
	  (([file exists $value] && [file writable $value]) ||
	   [file writable [file dirname $value]]) &&
	  [file tail [file dirname [file dirname $value]]] eq "Levels" &&
	  [isvalidmountpoint [file dirname 
				   [file dirname [file dirname $value]]]]} {
	return $value
      } else {
	error "Expected a valid space file for $option, but got $value"
      }
    }	
  }
  
  snit::widget MapEditor {
    option -template -readonly yes -default {}
    ::RolePlayingDB3::OpenFilename
    typevariable defaultfilename "map.rpg"
    variable currentFilename
    variable currentBaseFilename
    variable isdirty no
    method setdirty {} {set isdirty yes}
    variable levelEditors [list]
    variable path
    variable tempfile
    variable needmediatreeupdated no

    component banner
    component toolbar
    component panes
    component   sbpane
    component     sbpanes
    component       levelpane
    component	      leveltree
    component       mediapane
    component	      mediatree
    component   mappane
    component     mapframe

    typevariable filetypes {
      {{Map Files} {.rpg}             }
      {{All Files}        *           }
    }
    typemethod myfiletypes {} {return $filetypes}
    typecomponent _editDialog
    typevariable bannerImage
    typevariable dialogIcon
    typemethod   getDialogIcon {} {return $dialogIcon}
    typevariable bannerBackground #a2de86
    typevariable mapTemplateXML {<?xml version="1.0" ?>
<rpgv3:Map xmlns:rpgv3="http://www.deepsoft.com/roleplayingdb/v3xmlns" 
	   name="Map" >Global Map Information
  <rpgv3:Field name="Name" type="Word / Short Phrase" updatable="yes" />
  <rpgv3:Field name="Campaign" type="Word / Short Phrase" updatable="yes" />
  <rpgv3:Field name="Game Master" type="Word / Short Phrase" updatable="yes" />
  <rpgv3:Field name="Space Type" type="Enumerated Type" values="Square Hexigonal" updatable="no" />
  <rpgv3:Field name="Short Description" type="Long Text" updatable="yes" />
  <rpgv3:Field name="Long Description" type="Document" updatable="yes" />
</rpgv3:Map>
    }
    typeconstructor {
      set bannerImage [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						 MapBanner.png]]
      set dialogIcon [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						 MapDialogIcon.png]]
      set _editDialog {}
    }
    typemethod _createEditDialog {} {
      if {"$_editDialog" ne "" && [winfo exists "$_editDialog"]} {return}
      set _editDialog [Dialog .editMapEditorDialog -image $dialogIcon \
				-cancel 2 -default 0 -modal local \
				-parent . -side bottom \
				-title "Edit Map" \
				-transient yes]
      $_editDialog add -name new    -text {Create}
      $_editDialog add -name open   -text {Open}  
      $_editDialog add -name cancel -text {Cancel}
      pack [message [$_editDialog getframe].message \
	-text "Create a new Map file or\nopen an existing Map file?" \
	-aspect 500] -fill both
    }
    typemethod edit {args} {
      $type _createEditDialog
      set answer [$_editDialog draw]
      switch $answer {
        0 {
	  $type new -template $mapTemplateXML
	}
	1 {
	  $type open
	}
	2 {return}
      }
    }
    method getfile {} {return "$currentFilename"}
    typemethod new {args} {
      set templateXML [from args -template $mapTemplateXML]
      set parent [from args -parent .]

      set newTop [RolePlayingDB3::RPGToplevel \
	.map%AUTO% \
	-mainframeconstructor $type \
	-mainframetemplate $templateXML \
	-class MapEditor]
    }
    method opennew {} {
      set currentFilename {}
      set currentBaseFilename *noname*
      set path [$type genname]
      set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      while {[file exists $tempfile]} {
	set path [$type genname]
	set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      }
      vfs::mk4::Mount $tempfile /$path
      file mkdir [file join /$path Levels]
      close [open [file join /$path Levels flag] w]
      file mkdir [file join /$path media]
      close [open [file join /$path media flag] w]
      file mkdir [file join /$path xml]
      close [open [file join /$path xml flag] w]
      [winfo toplevel $win] configure -title "Map Edit: $currentBaseFilename"
    }
    method new {} {
      $type new -parent $win -template $mapTemplateXML
    }
    typevariable genindex 0
    typemethod genname {} {
      incr genindex
      return [format {MAP%05d} $genindex]
    }
    typemethod open {args} {
      set parent [from args -parent .]
      set like [from args -like $defaultfilename]
      set currentFilename [tk_getOpenFile -defaultextension .rpg \
				-filetypes $filetypes \
				-initialdir [file dirname $like] \
				-initialfile $like \
				-parent $parent \
				-title "File to open"]
      if {"$currentFilename" eq ""} {return}
      set newTop [RolePlayingDB3::RPGToplevel \
			.map%AUTO% \
			-mainframeconstructor $type \
			-mainframetemplate {} \
			-openfilename $currentFilename \
			-class MapEditor]
    }
    method openold {_filename} {
      set path [$type genname]
      set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      while {[file exists $tempfile]} {
	set path [$type genname]
	set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      }
      vfs::mk4::Mount $tempfile /$path
      set currentFilename $_filename 
      set currentBaseFilename [file tail $currentFilename]
      set inpath [$type genname]
      vfs::zip::Mount $currentFilename $inpath
      if {[catch {file copy [file join $inpath Levels] /$path}]} {
	file mkdir [file join /$path Levels]
	close [open [file join /$path Levels flag] w]
      }
      if {[catch {file copy [file join $inpath xml] /$path}]} {
	file mkdir [file join /$path xml]
	close [open [file join /$path xml flag] w]
      }
      if {[catch {file copy [file join $inpath media] /$path}]} {
	file mkdir [file join /$path media]
	close [open [file join /$path media flag] w]
      }
      vfs::unmount $inpath
      [winfo toplevel $win] configure -title "Map Editor: $currentBaseFilename"
    }
    method save {} {
      $self saveas $currentFilename
    }
    method saveas {{_filename {}}} {
      if {"$_filename" eq {}} {
	set _filename [tk_getSaveFile -defaultextension .rpg \
				      -filetypes $filetypes \
				      -initialdir [file dirname $currentFilename] \
				      -initialfile $currentFilename \
				      -parent $win \
				      -title "Save As File"]
      }
      if {"$_filename" eq {}} {return}
      foreach le $levelEditors {
	$le checksave
      }
      if {$isdirty} {$self recreateXML}
      ::ZipArchive createZipFromDirtree $_filename /$path \
				-comment "RPGV3 Map Bundle"
      set isdirty no
      if {"$currentFilename" ne "$_filename"} {
	set currentFilename $_filename
	set currentBaseFilename [file tail $currentFilename]
	[winfo toplevel $win] configure -title "Map Edit: $currentBaseFilename]"
      }
    }
    method print {} {
    }
    method close {} {
      set dirtylevels no
      foreach le $levelEditors {
	if {[$le getdirty]} {
	  set dirtylevels yes
	  break
	}
      }
      if {$isdirty || $dirtylevels} {
	set ans [tk_messageBox -type yesnocancel -icon question -parent $win \
				-message "Save data before closing window?"]
	switch $ans {
	  yes {$self save}
	  cancel {return}
	  no {}
        }
      }
      vfs::unmount /$path
      file delete $tempfile
      destroy [winfo toplevel $win]
    }
    constructor {args} {
      set options(-template) [from args -template]
      set options(-openfilename) [from args -openfilename]
      if {"$options(-openfilename)" eq "" && "$options(-template)" ne ""} {
        $self opennew
        set XML $options(-template)
	set xmlfile [file join /$path xml map.xml]
	set isnew yes
      } elseif {"$options(-openfilename)" ne ""} {
        $self openold $options(-openfilename)
	set xmlfile {}
        if {[catch {open [file join /$path xml map.xml] r} shfp]} {
	  error "Illformed map bundle: map.xml cannot be opened: $shfp"
	}
	set XML [read $shfp]
        close $shfp
	set isnew no
      } else {
	error "Neither -template nor -openfilename was passed!"
      }
      install banner using Label $win.banner \
			-image $bannerImage -anchor w \
			-background $bannerBackground
      pack $banner -fill x
      install toolbar using ButtonBox $win.toolbar -orient horizontal \
						   -homogeneous no
      pack $toolbar -fill x
      $toolbar add -name newlevel -text {New Level} \
				  -command [mymethod _newlevel]
      $toolbar add -name dellevel -text {Delete Level} \
				  -command [mymethod _deletelevel]
      $toolbar add -name editlevel -text {Edit Level} \
				   -command [mymethod _editlevel]
      $toolbar add -name addmedia -text {Add Media} \
				  -command [mymethod _addmedia]
      $toolbar add -name newmediafolder -text {New Media Folder} \
				  -command [mymethod _newmediafolder]
      $toolbar add -name delmedia -text {Delete Media} \
				  -command [mymethod _delmedia]
      $toolbar add -name delmediafolder -text {Delete Media Folder} \
      				  -command [mymethod _delmediafolder]
      install panes using PanedWindow $win.panes -side top
      pack $panes -fill both -expand yes
      set sbpane [$panes add -weight 1]
      install sbpanes using PanedWindow $sbpane.sbpanes -side right
      pack $sbpanes -fill both -expand yes
      set levelpane [$sbpanes add -weight 1]
      install leveltree using ::RolePlayingDB3::LabeledDirTree \
		$levelpane.leveltree -showextension no -filepattern *.xml \
				     -label "Levels" -nofiles yes -opendirs no \
				     -sortfunction [mymethod _sortlevelsbydepth]
      pack $leveltree -expand yes -fill both
      set mediapane [$sbpanes add -weight 1]
      install mediatree using ::RolePlayingDB3::LabeledDirTree \
		$mediapane.mediatree -showextension yes -filepattern * \
				     -label "Media"
      pack $mediatree -expand yes -fill both
      set mappane [$panes add -weight 5]
      install mapframe using ::RolePlayingDB3::XMLContentEditor \
			$mappane.mapframe -xml $XML -isnewobject $isnew \
					  -dirtyvariable [myvar isdirty] \
					  -filewidgethandler [mymethod _filewidgethandler] \
					  -xmlfile $xmlfile -basedirectory /$path
      pack $mapframe -fill both -expand yes
      $leveltree configure -directory [file join /$path Levels]
      $mediatree configure -directory [file join /$path media]
      $self configurelist $args
    }
    method _sortlevelsbydepth {la lb} {
#      puts stderr "*** $self _sortlevelsbydepth $la $lb"
      if {[catch {open [file join $la levelinfo.xml] r} lfp]} {
	error "Illformed map bundle: [file join $la levelinfo.xml] cannot be opened: $lfp"
      }
      set xmla [read $lfp]
      close $lfp
      if {[catch {open [file join $lb levelinfo.xml] r} lfp]} {
	error "Illformed map bundle: [file join $lb levelinfo.xml] cannot be opened: $lfp"
      }
      set xmlb [read $lfp]
      close $lfp
      set a_depth [::RolePlayingDB3::XMLContentEditor ExtractTagValue $xmla [mymethod _matchfield Depth] 0]
#      puts stderr "*** $self _sortlevelsbydepth: a_depth = $a_depth"
      set b_depth [::RolePlayingDB3::XMLContentEditor ExtractTagValue $xmlb [mymethod _matchfield Depth] 0]
#      puts stderr "*** $self _sortlevelsbydepth: b_depth = $b_depth"
      set comp [expr {$a_depth - $b_depth}]
#      puts stderr "*** $self _sortlevelsbydepth: comp = $comp"
      if {$comp == 0} {
	return [string compare -nocase [file tail $la] [file tail $lb]]
      } else {
	return $comp
      }
    }
    method _matchfield {matchname tag attrlist arglist} {
#      puts stderr "*** $self _matchfield: matchname = $matchname, tag = $tag, attrlist = $attrlist"
      if {[string totitle $tag] eq "Field"} {
        foreach {n v} $attrlist {
	  if {"$n" eq "name" && [string totitle "$v"] eq "$matchname"} {
	    return yes
	  }
	}
      }
      return no
    }
    method recreateXML {} {
      set needmediatreeupdated no
      $mapframe recreateXML [file join /$path xml map.xml]
      if {$needmediatreeupdated} {$self updatemediatree}
    }
    method updateleveltree {} {
      $leveltree redrawdirtree
    }
    method updatemediatree {} {
      $mediatree redrawdirtree
    }
    method _filewidgethandler {curfile} {
      if {[file pathtype "$curfile"] eq "relative" &&
	  "media" eq [lindex [file split $curfile] 0]} {
	return "$curfile"
      } else {
	file copy "$curfile" [file join /$path media [file tail $curfile]]
	set needmediatreeupdated yes
	return "[file join media [file tail $curfile]]"
      }
    }
    method _newlevel {args} {
      set parent [from args -parent $win]
      set newleveleditor [::RolePlayingDB3::LevelEditor new \
						-mapbundlemountpoint /$path \
						-parent $parent \
						-mapeditor $self]
      if {"$newleveleditor" ne ""} {
	set isdirty yes
	$self updateleveltree
	lappend levelEditors $newleveleditor
      }
    }
    method _deletelevel {} {
      if {[llength [glob -nocomplain -type d [file join /$path Levels *]]] == 0} {return}
      set level [tk_chooseDirectory \
			-initialdir [file join /$path Levels] \
			-parent $win -title "Level to delete" \
			-mustexist yes]
      if {"$level" eq ""} {return}
      if {[file system [file dirname $level]] ne [file system /$path]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join /$path Levels]]/* $level]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the Levels folder!"
	return
      }
      if {"$level" eq [file normalize [file join /$path Levels]]} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot delete the Levels folder itself!"
	return
      }
      set lelist $levelEditors
      foreach le $levelEditors {
        if {"$level" eq "[$le getleveldir]"} {
	  $le close -dontsave yes
	  set leindex [lsearch -exact $lelist $le]
	  if {$leindex >= 0} {set lelist [lreplace $lelist $leindex $leindex]}
	}
      }
      set levelEditors $lelist
      deletetree $level
      $self updateleveltree
      set isdirty yes
    }
    method _editlevel {args} {
      set parent [from args -parent $win]
      if {[llength [glob -nocomplain -type d [file join /$path Levels *]]] == 0} {return}
      set level [tk_chooseDirectory \
			-initialdir [file join /$path Levels] \
			-parent $parent -title "Level to edit" \
			-mustexist yes]
      if {"$level" eq ""} {return}
      if {[file system [file dirname $level]] ne [file system /$path]} {
	tk_messageBox -parent $parent -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join /$path Levels]]/* $level]} {
	tk_messageBox -parent $parent -type ok -icon info -message "Opps, you stepped out of the Levels folder!"
	return
      }
      if {"$level" eq [file normalize [file join /$path Levels]]} {
	tk_messageBox -parent $parent -type ok -icon info -message "You cannot edit the Levels folder itself!"
	return
      }
      foreach le $levelEditors {
        if {"$level" eq "[$le getleveldir]"} {
	  wm deinconify $le
	  raise $le
	  return
        }
      }
      set newleveleditor [::RolePlayingDB3::LevelEditor open \
						-mapbundlemountpoint /$path \
						-leveldir $level \
						-mapeditor $self \
						-parent $parent]
      if {"$newleveleditor" ne ""} {
	lappend levelEditors $newleveleditor
      }
    }
    method removeeditor {le} {
      set leindex [lsearch -exact $levelEditors $le]
      if {$leindex >= 0} {
	set levelEditors [lreplace $levelEditors $leindex $leindex]
      }
    }
    method _addmedia {} {
      set sourcefile [tk_getOpenFile -parent $win -title "Source file" \
      				     -initialdir [file dirname $currentFilename]]
      if {"$sourcefile" eq ""} {return}
      set destfile   [tk_getSaveFile -parent $win -title "Destination file" \
				     -initialdir [file join /$path media] \
				     -initialfile [file tail $sourcefile]]
      if {"$destfile" eq ""} {return}
      if {[file system [file dirname $destfile]] ne [file system /$path]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join /$path media]]/* $destfile]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the media folder!"
	return
      }
      file copy $sourcefile $destfile
      $self updatemediatree
      set isdirty yes
    }
    method _newmediafolder {} {
      set folder [tk_chooseDirectory \
			-initialdir [file join /$path media] \
			-parent $win -title "New Folder"]
      if {"$folder" eq ""} {return}
      if {[file system [file dirname $folder]] ne [file system /$path]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join /$path media]]/* $folder]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the media folder!"
	return
      }
      if {[file exists "$folder"] && ![file isdirectory "$folder"]} {
	tk_messageBox -parent $win -type ok -icon info -message "$folder exists and is not a folder!"
	return
      }
      if {[file exists "$folder"]} {
	tk_messageBox -parent $win -type ok -icon info -message "$folder already exists."
	return
      }
      file mkdir $folder
      close [open [file join $folder flag] w]
      $self updatemediatree
      set isdirty yes
    }
    method _delmedia {} {
      set destfile   [tk_getOpenFile -parent $win -title "File to delete" \
				     -initialdir [file join /$path media]]

      if {"$destfile" eq ""} {return}
      if {[file tail $destfile] eq "flag"} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot delete place holder (flag) files!"
	return
      }
      if {[file system $destfile] ne [file system /$path]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join /$path media]]/* $destfile]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the media folder!"
	return
      }
      file delete $destfile
      $self updatemediatree
      set isdirty yes
    }
    proc deletetree {dir} {
      foreach d [glob -nocomplain -type d [file join $dir *]] {
	deltree $d
      }
      foreach f [glob -nocomplain [file join $dir *]] {
	file delete $f
      }
      file delete $dir
    }
    method _delmediafolder {} {
      set folder [tk_chooseDirectory \
			-initialdir [file join /$path media] \
			-parent $win -title "Folder to delete" \
			-mustexist yes]
      if {"$folder" eq ""} {return}
      if {[file system [file dirname $folder]] ne [file system /$path]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join /$path media]]/* $folder]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the media folder!"
	return
      }
      if {"$folder" eq [file normalize [file join /$path media]]} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot delete the media folder itself!"
	return
      }
      if {[llength [glob -nocomplain [file join $folder *]]] > 1} {
	set ans [tk_messageBox -parent $win -type yesno -default no \
			-icon question -message "$folder is not empty! Are sure you want to delete it?"]
	if {"$ans" eq "no"} {return}
      }
#      puts stderr "*** $self _delmediafolder: folder = $folder"
      deletetree $folder
      $self updatemediatree
      set isdirty yes
    }
  }
  snit::widget LevelEditor {
    option -template -readonly yes -default {}
    ::RolePlayingDB3::MapBundleMountPoint
    ::RolePlayingDB3::MapEditorOption
    ::RolePlayingDB3::LevelDirOption
    variable currentLevelDir
    method getleveldir {} {return $currentLevelDir}
    variable isdirty no
    variable needmediatreeupdated
    variable spaceEditors [list]
    variable firstsave yes
    method setdirty {} {set isdirty yes}
    method getdirty {} {return $isdirty}
    method checksave {} {
      if {$isdirty} {
	$self recreateXML
	if {$firstsave} {
	  $options(-mapeditor) updateleveltree
	  set firstsave no
	}
      }
      foreach se $spaceEditors {
	$se checksave
      }
    }

    component banner
    component toolbar
    component panes  
    component   sbpane
    component     spacetree
    component   levelpane
    component     levelframe

    typevariable bannerImage
    typevariable bannerBackground #a2de86
    typevariable levelTemplateXML {<?xml version="1.0" ?>
<rpgv3:Level xmlns:rpgv3="http://www.deepsoft.com/roleplayingdb/v3xmlns"
	     name="Level" >Level Information
  <rpgv3:Field name="Title" type="Word / Short Phrase" updatable="yes" />
  <rpgv3:Field name="Depth" type="Whole Number" updatable="no" range="-50 50 1" />
  <rpgv3:Field name="Description" type="Long Text" updatable="yes" />
  <rpgv3:LevelMap name="Level Map">Level Map
    <rpgv3:Canvas side="left" id="map" />
    <rpgv3:Buttonbox side="right" orient="vertical" >
      <rpgv3:Bi label="Zoom In"  id="zoomin" />
      <rpgv3:Bi label="Zoom 1:1" id="zoom11" />
      <rpgv3:Bi label="Zoom Out" id="zoomout" />
    </rpgv3:Buttonbox >
  </rpgv3:LevelMap >  
</rpgv3:Level>
    }
    typeconstructor {
      set bannerImage [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						 LevelBanner.png]]
    }
    method new {} {
      $options(-mapeditor) _newlevel -parent $win
    }
    typemethod new {args} {
      set mapbundlemountpoint [from args -mapbundlemountpoint]
      set parent [from args -parent .]
      set mapeditor [from args -mapeditor]
      set leveldir [tk_chooseDirectory \
      			-initialdir [file join $mapbundlemountpoint Levels] \
			-parent $parent -title "Level to create"]
      if {"$leveldir" eq ""} {return ""}
      if {[file system [file dirname $leveldir]] ne [file system $mapbundlemountpoint]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join $mapbundlemountpoint Levels]]/* $leveldir]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the Levels folder!"
	return
      }
      if {"$leveldir" eq [file normalize [file join $mapbundlemountpoint Levels]]} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot use the Levels folder itself as a level!"
	return
      }
      if {[file exists "$leveldir"]} {
	tk_messageBox -parent $win -type ok -icon info -message "Level [file tail $leveldir] already exists!"
	return
      }
      set newTop [RolePlayingDB3::RPGToplevel \
		.level%AUTO% \
		-mainframeconstructor $type \
		-mainframetemplate $levelTemplateXML \
		-class LevelEditor \
		-mapbundlemountpoint $mapbundlemountpoint \
		-mapeditor $mapeditor -leveldir $leveldir -minwidth 800]
      return $newTop
    }
    method open {} {
      $options(-mapeditor) _editlevel -parent $win
    }
    typemethod open {args} {
      set mapbundlemountpoint [from args -mapbundlemountpoint]
      set leveldir [from args -leveldir]
      set mapeditor [from args -mapeditor]
      set parent [from args -parent .]

      set newTop [RolePlayingDB3::RPGToplevel \
		.level%AUTO% \
		-mainframeconstructor $type \
		-mainframetemplate {} \
		-class LevelEditor \
		-mapbundlemountpoint $mapbundlemountpoint \
		-mapeditor $mapeditor \
		-leveldir $leveldir -minwidth 800]
      return $newTop
    }
    method save {} {
      $options(-mapeditor) save
    }
    method saveas {{_filename {}}} {
      $options(-mapeditor) saveas $_filename
    }
    method print {} {
      $options(-mapeditor) print
    }
    method close {args} {
      set dontsave [from args -dontsave no]
      if {!$dontsave} {
	set dirtyspaces no
	foreach se $spaceEditors {
	  if {[$se getdirty]} {
	    set dirtyspaces yes
	    break
	  }
        }
	if {$isdirty || $dirtyspaces} {
	  set ans [tk_messageBox -type yesnocancel -icon question -parent $win \
				-message "Save data before closing window?"]
	  switch $ans {
	    yes {$self checksave}
	    cancel {return}
	    no {}
	  }
        }
      }
      $options(-mapeditor) removeeditor [winfo toplevel $win]
      destroy [winfo toplevel $win]
    }
    constructor {args} {
      set options(-template) [from args -template]
      set options(-mapbundlemountpoint) [from args -mapbundlemountpoint]
      set options(-mapeditor) [from args -mapeditor]
      set options(-leveldir) [from args -leveldir]

      set currentLevelDir [file tail $options(-leveldir)]
      [winfo toplevel $win] configure -title "Level Edit: $currentLevelDir"
      if {"$options(-template)" ne "" && ![file exists $options(-leveldir)]} {
	file mkdir $options(-leveldir)
	close [open [file join $options(-leveldir) flag] w]
	set XML $options(-template)
	set xmlfile [file join $options(-leveldir) levelinfo.xml]
	set firstsave yes
	set isnew yes
      } elseif {[file exists $options(-leveldir)]} {
	set xmlfile {}
	if {[catch {open [file join $options(-leveldir) levelinfo.xml] r} shfp]} {
	  error "Illformed map bundle: [file join $currentLevelDir levelinfo.xml] cannot be opened: $shfp"
	}
	set XML [read $shfp]
	close $shfp
	set firstsave no
	set isnew no
      } else {
	error "Neither -template nor a valid -leveldir was passed!"
      }
      install banner using Label $win.banner \
			-image $bannerImage -anchor w \
			-background $bannerBackground
      pack $banner -fill x  
      install toolbar using ButtonBox $win.toolbar -orient horizontal \
						   -homogeneous no
      pack $toolbar -fill x
      $toolbar add -name newspace -text {New Space} \
				  -command [mymethod _newspace]
      $toolbar add -name delspace -text {Delete Space} \
				  -command [mymethod _deletespace]
      $toolbar add -name editspace -text {Edit Space} \
				   -command [mymethod _editspace]
      install panes using PanedWindow $win.panes -side top
      pack $panes -fill both -expand yes
      set sbpane [$panes add -weight 1]
      install spacetree using ::RolePlayingDB3::LabeledDirTree \
		$sbpane.spacetree -showextension no -filepattern *.xml \
				  -label "Spaces"
      pack $spacetree -expand yes -fill both
      set levelpane [$panes add -weight 5]
      install levelframe using ::RolePlayingDB3::XMLContentEditor \
			$levelpane.levelframe -xml $XML -isnewobject $isnew \
					      -dirtyvariable [myvar isdirty] \
					      -filewidgethandler [mymethod _filewidgethandler] \
					      -xmlfile $xmlfile \
					      -buttoncommand [mymethod _button] \
					      -basedirectory $options(-mapbundlemountpoint)
      pack $levelframe -fill both -expand yes
      $spacetree configure -directory $options(-leveldir)
      $self configurelist $args
    }
    method _button {id} {
      switch $id {
	zoomin {
	}
	zoom11 {
	}
	zoomout {
	}
      }
    }
    method recreateXML {} {
      set needmediatreeupdated no
      $levelframe recreateXML [file join $options(-leveldir) levelinfo.xml]
      if {$needmediatreeupdated} {$options(-mapeditor) updatemediatree}
    }
    method _filewidgethandler {curfile} {
      if {[file pathtype "$curfile"] eq "relative" &&
	  "media" eq [lindex [file split $curfile] 0]} {
	return "$curfile"
      } else {
	file copy "$curfile" [file join $mapbundlemountpoint media [file tail $curfile]]
	set needmediatreeupdated yes
	return "[file join media [file tail $curfile]]"
      }
    }
    method removeeditor {se} {
      set seindex [lsearch -exact $spaceEditors $se]
      if {$seindex >= 0} {
	set spaceEditors [lreplace $spaceEditors $seindex $seindex]
      }
    }
    method _newspace {args} {
      set parent [from args -parent $win]
      set newspaceeditor [::RolePlayingDB3::SpaceEditor new \
				-mapbundlemountpoint $options(-mapbundlemountpoint) \
				-leveldir $options(-leveldir) \
				-parent $parent \
				-leveleditor $self]
      if {"$newspaceeditor" ne ""} {
	set isdirty yes
	$self updatespacetree
	lappend spaceEditors $newspaceeditor
      }
    }
    method _deletespace {} {
      if {[llength [glob -nocomplain -type f [file join $options(-leveldir) *.xml]]] == 1} {
	return
      }
      set space [tk_getOpenFile \
			-initialdir $options(-leveldir) \
			-parent $win -title "Space to delete" \
			-filetypes {{{Space Files} *.xml TEXT}}]
      if {"$space" eq ""} {return}
      if {[file system [file dirname $space]] ne [file system $options(-leveldir)]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join $options(-leveldir)]]/* $space]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of this level's folder!"
	return
      }
      if {"$space" eq [file normalize [file join $options(-leveldir) levelinfo.xml]]} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot delete the levelinfo file!"
	return
      }
      set selist $spaceEditors
      foreach se $spaceEditors {
	if {"$space" eq "[$se getspacefile]"} {
	  $se close -dontsave yes
	  set seindex [lsearch -exact $selist $se]
	  if {$seindex >= 0} {set selist [lreplace $seindex $seindex $seindex]}
	}
      }
      set spaceEditors $selist
      file delete $space
      $self updatespacetree
      set isdirty yes      
    }
    method _editspace {args} {
      set parent [from args -parent $win]
      if {[llength [glob -nocomplain -type f [file join $options(-leveldir) *.xml]]] == 1} {
	return
      }
      set space [tk_getOpenFile \
			-initialdir $options(-leveldir) \
			-parent $win -title "Space to delete" \
			-filetypes {{{Space Files} *.xml TEXT}}]
      if {"$space" eq ""} {return}
      if {[file system [file dirname $space]] ne [file system $options(-leveldir)]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join $options(-leveldir)]]/* $space]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of this level's folder!"
	return
      }
      if {"$space" eq [file normalize [file join $options(-leveldir) levelinfo.xml]]} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot use the space editor on the levelinfo file!"
	return
      }
      foreach se $spaceEditors {
	if {"$space" eq "[$se getspacefile]"} {
	  wm deinconify $se
	  raise $se
	  return
	}
      }
      set newspaceeditor [::RolePlayingDB3::SpaceEditor new \
				-mapbundlemountpoint $options(-mapbundlemountpoint) \
				-leveldir $options(-leveldir) \
				-parent $parent -spacefile $space \
				-leveleditor $self]
      if {"$newspaceeditor" ne ""} {
	lappend spaceEditors $newspaceeditor
      }
    }
  }
  snit::widget SpaceEditor {
    option -template -readonly yes -default {}
    ::RolePlayingDB3::MapBundleMountPoint
    ::RolePlayingDB3::LevelEditorOption
    ::RolePlayingDB3::SpaceFileOption
    variable currentSpaceFile
    method getspacefile {} {return $currentSpaceFile}
    variable firstsave yes
    variable isdirty no
    method setdirty {} {set isdirty yes}
    method checksave {} {
      if {$isdirty} {$self recreateXML}
    }

    component banner
    component toolbar
    component panes
    component   sbpane
    component     sbpanes
    component       itempane
    component         itemlist
    component	    exitpane
    component         exitlist
    component   spacepane
    component     spaceframe

    typevariable bannerImage
    typevariable bannerBackground #a2de86
    typevariable levelTemplateXML {<?xml version="1.0" ?>
<rpgv3:Space xmlns:rpgv3="http://www.deepsoft.com/roleplayingdb/v3xmlns"
	     name="Space" >Space Information
  <rpgv3:Field name="Title" type="Word / Short Phrase" updatable="yes" />
  <rpgv3:Field name="X Coord" type="Whole Number" updatable="no" range="-1000 1000 1" />
  <rpgv3:Field name="Y Coord" type="Whole Number" updatable="no" range="-1000 1000 1" />
  <rpgv3:Field name="Description" type="Long Text" updatable="yes" />
  <rpgv3:SpaceContents name="Space Contents">Space Contents
  <rpgv3:Items name="Items" side="right">List of items
    <rpgv3:List id="itemlist" />
  </rpgv3:Items>
  <rpgv3:Exits name="Exits" side="left" >List of exits
    <rpgv3:List id="exitlist" />
  </rpgv3:Exits>
  <rpgv3:Buttonbox orient="horizontal">
    <rpgv3:Bi label="Add New Item" id="addnewitem" />
    <rpgv3:Bi label="Edit Item"    id="edititem" />
    <rpgv3:Bi label="Delete Item"  id="deleteitem" />
    <rpgv3:Bi label="Add New Exit" id="addnewexit" />
    <rpgv3:Bi label="Edit Exit"    id="editexit" />
    <rpgv3:Bi label="Delete Exit"  id="deleteexit" />
  </rpgv3:Buttonbox>
</rpgv3:Space>
    }
    typeconstructor {
      set bannerImage [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						 SpaceBanner.png]]
    }
    method new {} {
      $options(-leveleditor) _newspace -parent $win
    }
    typemethod new {args} {
      set mapbundlemountpoint [from args -mapbundlemountpoint]
      set parent [from args -parent .]
      set leveleditor [from args -leveleditor]
      set leveldir [from args -leveldir]
      set space [tk_getSaveFile \
			-initialdir $options(-leveldir) \
			-parent $win -title "Space to create" \
			-filetypes {{{Space Files} *.xml TEXT}}]
      if {"$space" eq ""} {return}
      if {[file system [file dirname $space]] ne [file system $options(-leveldir)]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join $options(-leveldir)]]/* $space]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of this level's folder!"
	return
      }
      if {"$space" eq [file normalize [file join $options(-leveldir) levelinfo.xml]]} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot delete the levelinfo file!"
	return
      }
      if {[file exists "$space"]} {
	tk_messageBox -parent $win -type ok -icon info -message "Space [file rootname [file tail $space]] already exists!"
        return
      }
      set newTop [RolePlayingDB3::RPGToplevel \
		.space%AUTO% \
		-mainframeconstructor $type \
		-mainframetemplate $spaceTemplateXML \
		-class SpaceEditor \
		-mapbundlemountpoint $mapbundlemountpoint \
		-leveleditor $leveleditor -leveldir $leveldir -minwidth 800]
      return $newTop
    }
    method open {} {
      $options(-leveleditor) _editspace -parent $win
    }
    typemethod open {args} {
      set mapbundlemountpoint [from args -mapbundlemountpoint]
      set parent [from args -parent .]
      set leveleditor [from args -leveleditor]
      set leveldir [from args -leveldir]
      set space [from args -spacefile]
      set newTop [RolePlayingDB3::RPGToplevel \
		.space%AUTO% \
		-mainframeconstructor $type \
		-mainframetemplate $spaceTemplateXML \
		-class SpaceEditor -spacefile $space \
		-mapbundlemountpoint $mapbundlemountpoint \
		-leveleditor $leveleditor -leveldir $leveldir -minwidth 800]
      return $newTop
    }
    method save {} {
      $options(-leveleditor) save
    }
    method saveas {{_filename {}}} {
      $options(-leveleditor) saveas $_filename
    }
    method print {} {
      $options(-leveleditor) print
    }
    method close {args} {
      set dontsave [from args -dontsave no]
      if {!$dontsave} {
	if {$isdirty} {
	  set ans [tk_messageBox -type yesnocancel -icon question -parent $win \
				-message "Save data before closing window?"]
	  switch $ans {
	    yes {$self checksave}
	    cancel {return}
	    no {}
	  }
	}
      }
      $options(-leveleditor) removeeditor [winfo toplevel $win]
      destroy [winfo toplevel $win]
    }
    constructor {args} {
    }
  }
}

package provide RPGMapLevelSpace 1.0
