#* 
#* ------------------------------------------------------------------
#* Role PlayingDB V3.0 by Deepwoods Software
#* ------------------------------------------------------------------
#* RPGSheetEdit.tcl - Generic Sheet Editor
#* Created by Robert Heller on Mon Aug 31 18:13:04 2009
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
package require BWLabelComboBox

namespace eval RolePlayingDB3 {
  snit::widget SheetEdit {
    option -template -readonly yes -default {}
    option {-sheetclass sheetClass SheetClass} \
		-readonly yes -default Character \
		-type { snit::enum -values {Character Dressing Monster Spell 
					    Treasure TrickTrap} }
    ::RolePlayingDB3::OpenFilename
    variable currentFilename
    variable currentBaseFilename
    variable isdirty no
    method setdirty {} {set isdirty yes}
    variable path
    variable tempfile
    variable sheetClass
    variable nodeStack
    variable nodeTree -array {}
    variable parseMode
    variable fileWidgets -array {}
    
    component banner
    component toolbar
    component sheetsw
    component   sheetframe

    typevariable filetypes {
      {{Sheet Files} {.rpg}		}
      {{All Files}        *             }
    }
    typemethod myfiletypes {} {return $filetypes}
    typecomponent _editDialog
    typecomponent  sheetTemplateFE
    typevariable bannerImage -array {}
    typevariable dialogIcon  -array {}
    typevariable bannerBackgrounds -array {
	Character #e52b2a
	Dressing  #91938e
	Monster   #85c152
	Spell     #a2dd84
	Treasure  #e1eb8f
	TrickTrap #95d9d7
    }
    typevariable editDialogIcon {}
    typeconstructor {
      foreach theclass {Character Dressing Monster Spell Treasure TrickTrap} {
	set bannerImage($theclass) [image create photo \
					-file [file join \
						$::RolePlayingDB3::ImageDir \
						${theclass}Banner.png]]
	set dialogIcon($theclass) [image create photo \
					-file [file join \
						$::RolePlayingDB3::ImageDir \
						${theclass}DialogIcon.png]]
	set editDialogIcon [image create photo \
					-file [file join \
						$::RolePlayingDB3::ImageDir \
						SheetEditDialogIcon.png]]
      }
      set _editDialog {}
    }
    typemethod _createEditDialog {} {
      if {"$_editDialog" ne "" && [winfo exists "$_editDialog"]} {return}
      set _editDialog [Dialog .editSheetEditDialog -image $editDialogIcon \
					-cancel 2 -default 0 -modal local \
					-parent . -side bottom \
					-title "Edit Sheet" \
					-transient yes]
      $_editDialog add -name new    -text {Create}
      $_editDialog add -name open   -text {Open}  
      $_editDialog add -name cancel -text {Cancel}
      pack [message [$_editDialog getframe].message \
			-text "Create a new Sheet file or\nopen an existing Sheet file?" \
			-aspect 500] -fill both
      set sheetTemplateFE [FileEntry \
				[$_editDialog getframe].sheetTemplateFE \
				-filedialog open \
				-filetypes { {{Template XML}   {.xml} TEXT}
					     {{All Text Files}     *  TEXT} } \
				-defaultextension .xml \
				-title {Template XML file}]
      pack $sheetTemplateFE -fill x
    }
    typemethod edit {args} {
      set sheetclass [from args -sheetclass]
      $type _createEditDialog
      set templateFile [::RolePlayingDB3::Configuration getoption Template]
      set mp [file rootname [file tail $templateFile]]
      vfs::zip::Mount $templateFile $mp
      set t1 [lindex [lsort -dictionary [glob -nocomplain \
						[file join $mp $sheetclass \
							*.xml]]] 0]
      $sheetTemplateFE configure -text "$t1"
      set answer [$_editDialog draw]
      switch $answer {
	0 {
	    set templateFile [$sheetTemplateFE cget -text]
	    if {[catch {open $templateFile r} tfp]} {
	      tk_messageBox -type ok -icon error -message "Could not open $templateFile: $tfp"
	      vfs::unmount $mp
	      return
	    }
	    set templateXML [read $tfp]
	    close $tfp
	    vfs::unmount $mp
	    $type new -template $templateXML -sheetclass $sheetclass
	}
	1 {
	    vfs::unmount $mp
	    $type open -sheetclass $sheetclass
	}
	2 {return}
      }
    }
    method getfile {} {return "$currentFilename"}
    typemethod new {args} {
      set templateXML [from args -template]
      set sheetclass [from args -sheetclass]
      set parent [from args -parent .]

      if {"$templateXML" eq ""} {
        set templateFile [::RolePlayingDB3::Configuration getoption Template]
        set mp [file rootname [file tail $templateFile]]
        vfs::zip::Mount $templateFile $mp
	set template [tk_getOpenFile -defaultextension .xml \
				     -filetypes { {{Template XML}   {.xml} TEXT}
						  {{All Text Files}     *  TEXT} } \
				     -initialdir [file join $mp $sheetclass] \
				     -parent $parent \
				     -title "Template for new $sheetclass"]
	if {"$template" eq ""} {return}
	if {[catch {open $template r} tfp]} {
	  tk_messageBox -type ok -icon error -message "Could not open $template: $tfp"
	  vfs::unmount $mp
	  return
	}
	set templateXML [read $tfp]
	close $tfp
	vfs::unmount $mp
      }
      set newTop [RolePlayingDB3::RPGToplevel \
			.[string tolower $sheetclass]%AUTO% \
			-mainframeconstructor $type \
			-mainframetemplate $templateXML \
			-sheetclass $sheetclass \
			-class ${sheetclass}Editor]
    }
    method opennew {} {
      set currentFilename {}
      set currentBaseFilename *noname*
      set path [$type genname $options(-sheetclass)]
      set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      while {[file exists $tempfile]} {
	set path [$type genname $options(-sheetclass)]
	set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      }
      vfs::mk4::Mount $tempfile /$path
      file mkdir [file join /$path media]
      close [open [file join /$path media flag] w]
      file mkdir [file join /$path xml]
      close [open [file join /$path xml flag] w]
      [winfo toplevel $win] configure -title "$options(-sheetclass) Edit: $currentBaseFilename"
    }
    method new {} {
      $type new -parent $win -sheetclass $options(-sheetclass) \
		-template $options(-template)
    }
    typevariable genindex 0
    typemethod genname {class} {
      incr genindex
      return [format {%s%05d} [string toupper $class] $genindex]
    }
    method open {} {
      $type open -parent $win -sheetclass $options(-sheetclass)\
		 -like $currentFilename
    }
    typemethod open {args} {
      set sheetclass [from args -sheetclass]
      set parent [from args -parent .]
      set like [from args -like [string tolower $sheetclass].rpg]
      if {"$like" eq ""} {set like [string tolower $sheetclass].rpg}

      set currentFilename [tk_getOpenFile -defaultextension .rpg \
				-filetypes $filetypes \
				-initialdir [file dirname $like] \
				-initialfile $like \
				-parent $parent \
				-title "File to open"]
      if {"$currentFilename" eq ""} {return}
      set newTop [RolePlayingDB3::RPGToplevel \
			.[string tolower $sheetclass]%AUTO% \
			-mainframeconstructor $type \
			-mainframetemplate {} \
			-sheetclass $sheetclass \
			-openfilename $currentFilename \
			-class ${sheetclass}Editor]
    }
    method openold {_filename} {
      set path [$type genname $options(-sheetclass)]
      set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      while {[file exists $tempfile]} {
	set path [$type genname $options(-sheetclass)]
	set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      }
      vfs::mk4::Mount $tempfile /$path
      set currentFilename $_filename 
      set currentBaseFilename [file tail $currentFilename]
      set inpath [$type genname $options(-sheetclass)]
      vfs::zip::Mount $currentFilename $inpath
      if {[catch {file copy [file join $inpath media] /$path}]} {
	file mkdir [file join /$path media]
	close [open [file join /$path media flag] w]
      }
      if {[catch {file copy [file join $inpath xml] /$path}]} {
	file mkdir [file join /$path xml]
      }
      vfs::unmount $inpath
      [winfo toplevel $win] configure -title "$options(-sheetclass) $currentBaseFilename"
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
      if {$isdirty} {$self recreateXML}
      ::ZipArchive createZipFromDirtree $_filename /$path \
				-comment "RPGV3 $options(-sheetclass) Bundle"
      set isdirty no
      if {"$currentFilename" ne "$_filename"} {
	set currentFilename $_filename
	set currentBaseFilename [file tail $currentFilename]
	[winfo toplevel $win] configure -title "$options(-sheetclass) Edit: $currentBaseFilename]"
      }
    }
    method print {} {
    }
    method close {} {
      if {$isdirty} {
	set ans [tk_messageBox -type yesnocancel -icon question \
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
      set options(-sheetclass) [from args -sheetclass]
      set options(-openfilename) [from args -openfilename]
      if {"$options(-openfilename)" eq "" && "$options(-template)" ne ""} {
	$self opennew
	set xmlfile [file join /$path xml sheet.xml]
	set XML $options(-template)
	set isnew yes
      } elseif {"$options(-openfilename)" ne ""} {
	$self openold $options(-openfilename)
	set xmlfile {}
	if {[catch {open [file join /$path xml sheet.xml] r} shfp]} {
	  error "Illformed sheet bundle: sheet.xml cannot be opened: $shfp"
	}
	set XML [read $shfp]
	close $shfp
	set isnew no
      } else {
	error "Neither -template nor -openfilename was passed!"
      }
      install banner using Label $win.banner \
			-image $bannerImage($options(-sheetclass)) -anchor w \
			-background $bannerBackgrounds($options(-sheetclass))
      pack $banner -fill x
      install toolbar using ButtonBox $win.toolbar -orient horizontal \
						-homogeneous no
      pack $toolbar -fill x 
      ## Tools??? ##
      install sheetframe using ::RolePlayingDB3::XMLContentEditor \
			$win.sheetframe -xml $XML -isnewobject $isnew \
					-templatevariable [myvar options(-template)] \
					-dirtyvariable [myvar isdirty] \
					-filewidgethandler [mymethod _filewidgethandler] \
					-xmlfile $xmlfile -basedirectory /$path
      pack $sheetframe -fill both -expand yes
      $self configurelist $args
#      puts stderr "*** $type $self: parseMode = $parseMode"
#      if {"$parseMode" eq "file"} {
#	puts stderr "*** $type $self: options(-template) is:\n$options(-template)"
#      }
    }
    method recreateXML {} {
      $sheetframe recreateXML [file join /$path xml sheet.xml]
    }
    method _filewidgethandler {curfile} {
      if {[file pathtype "$curfile"] eq "relative" &&
	  "media" eq [lindex [file split $curfile] 0]} {
	return "$curfile"
      } else {
	file copy "$curfile" [file join /$path media [file tail $curfile]]
	return "[file join media [file tail $curfile]]"
      }
    }
  }
}



package provide RPGSheetEdit 1.0
