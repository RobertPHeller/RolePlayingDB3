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

## @addtogroup RPGSupport
# @{

package require vfs::zip
package require vfs::rpg
package require ZipArchive
package require ParseXML
package require RPGUtilities
#package require BWLabelComboBox
package require IconImage
package require Dialog
package require ButtonBox
package require LabelFrames
package require pdf4tcl
package require ScrollableFrame

namespace eval RolePlayingDB3 {
    ##
    
    snit::enum SheetClasses -values {Character Dressing Monster Spell Treasure 
        TrickTrap}
    ## @enum SheetClasses The defined sheet classes.
    snit::widget SheetEdit {
        ## Widget to edit a sheet.  Wraps around XMLContentEditor to edit a 
        # "sheet".  Gets installed into a standard RPG Toplevel frame -- not
        # normally installed directly.
        
        option -template -readonly yes -default {}
    
        option {-sheetclass sheetClass SheetClass} \
              -readonly yes -default Character \
              -type ::RolePlayingDB3::SheetClasses
        ::RolePlayingDB3::OpenFilename
        variable currentFilename
        ## @privatesection Holds current filename.
        variable currentBaseFilename
        ## Holds base filename.
        variable isdirty no
        ## Dirty flag
        method setdirty {} {
            ## Set dirty flag.
            
            #puts stderr "*** $self setdirty"
            set isdirty yes
        }
        variable path
        ## Holds the mount point path.
        variable tempfile
        ## Name of the temp (backing) file.
        variable sheetClass
        ## The sheet class.
        
        component banner
        ## Banner component.
        component toolbar
        ## Toolbar component.
        component sheetsw
        ## Sheet scrolling.
        component   sheetframe
        ## Sheet frame.
        
        typevariable filetypes {
            {{Sheet Files} {.rpg}		}
            {{All Files}        *             }
        }
        ## File types
        typemethod myfiletypes {} {
            ## Return the defined filetypes.
            # @returns the file types (for tk_getOpenFile and tk_getSaveFile).
            return $filetypes
        }
        typecomponent _editDialog
        ## Edit dialog.
        typecomponent  sheetTemplateLF
        ## Sheet template file.
        typecomponent    sheetTemplateE
        ## Sheet template entry.
        typecomponent    sheetTemplateB
        ## Sheet template button.
        typecomponent    sheetTemplateDialog
        ## Sheet template Dialog.
        typecomponent _getMediaFileDialog
        ## Get Media file Dialog.
        typecomponent _getTemplateFileDialog
        ## Get  Template file Dialog.
        typevariable bannerImage -array {}
        ## Banner images.
        typevariable dialogIcon  -array {}
        ## Dialog icon.
        typevariable bannerBackgrounds -array {
            Character #e52b2a
            Dressing  #91938e
            Monster   #85c152
            Spell     #bb4649
            Treasure  #e1eb8f
            TrickTrap #95d9d7
        }
        ## Banner background colors (match the banner images).
        typevariable openMediaFileDialogBanner -array {}
        ## Open Media file dialog banners.
        typevariable openTemplateFileDialogBanner {}
        ## Open template file dialog banner.
        typevariable editDialogIcon {}
        ## Edit dialog icon.
        typeconstructor {
            ## Type constructor: initialize type variables.
            
            foreach theclass {Character Dressing Monster Spell Treasure TrickTrap} {
                set bannerImage($theclass) [image create photo \
                                            -file [file join \
                                                   $::RolePlayingDB3::ImageDir \
                                                   ${theclass}Banner.png]]
                set dialogIcon($theclass) [image create photo \
                                           -file [file join \
                                                  $::RolePlayingDB3::ImageDir \
                                                  ${theclass}DialogIcon.png]]
                set openMediaFileDialogBanner($theclass) [image create photo \
                                                          -file [file join \
                                                                 $::RolePlayingDB3::ImageDir \
                                                                 ${theclass}OpenMediaFileBanner.png]]
            }
            set editDialogIcon [image create photo \
                                -file [file join \
                                       $::RolePlayingDB3::ImageDir \
                                       SheetEditDialogIcon.png]]
            set openTemplateFileDialogBanner [image create photo \
                                              -file [file join \
                                                     $::RolePlayingDB3::ImageDir \
                                                     OpenTemplateFileDialogBanner.png]]
            set _editDialog {}
        }
        typemethod createGetMediaFileDialog {} {
            ## Create the Get Media File Dialog.
            if {"$_getMediaFileDialog" ne "" && [winfo exists "$_getMediaFileDialog"]} {return}
            set _getMediaFileDialog [::RolePlayingDB3::chroot_getFile \
                                     .getSheetMediaFileDialog \
                                     -bannerimage $openMediaFileDialogBanner(Character) \
                                     -bannerbackground $bannerBackgrounds(Character)]
        }
        typemethod draw_getMediaFileDialog {args} {
            ## Draw the Get Media File Dialog.
            # @params ... Options: passed to Get Media File Dialog 
            # (see @ref RolePlayingDB3::chroot_getFile).
            
            $type createGetMediaFileDialog
            return [$_getMediaFileDialog draw {*}$args]
        }
        typemethod _createEditDialog {} {
            ## Create the Edit Dialog.
            
            if {"$_editDialog" ne "" && [winfo exists "$_editDialog"]} {return}
            set _editDialog [Dialog .editSheetEditDialog -image $editDialogIcon \
                             -cancel 2 -default 0 -modal local \
                             -parent . -side bottom \
                             -title "Edit Sheet" \
                             -transient yes]
            $_editDialog add new    -text {Create}
            $_editDialog add open   -text {Open}  
            $_editDialog add cancel -text {Cancel}
            pack [message [$_editDialog getframe].message \
                  -text "Create a new Sheet file or\nopen an existing Sheet file?" \
                  -aspect 500] -fill both
            set sheetTemplateLF [LabelFrame [$_editDialog getframe].sheetTemplateLF \
                                 -text {Template File:}]
            pack $sheetTemplateLF -fill x
            set sheetTemplateLF_f [$sheetTemplateLF getframe]
            set sheetTemplateE [tk::entry $sheetTemplateLF_f.sheetTemplateE]
            pack $sheetTemplateE -side left -fill x
            set sheetTemplateB [tk::button $sheetTemplateLF_f.sheetTemplateB \
      				-image [IconImage image openfold] \
				-command [mytypemethod _openTemplateFile]]
            pack $sheetTemplateB -side right
            set sheetTemplateDialog [::RolePlayingDB3::chroot_getFile \
                                     $sheetTemplateLF_f.sheetTemplateDialog \
                                     -bannerimage $openTemplateFileDialogBanner \
                                     -bannerbackground white \
                                     -saveoropen open \
                                     -filetypes { {{Template XML}   {.xml} TEXT}
                                     {{All Text Files}     *  TEXT} } \
                                       -defaultextension .xml \
                                       -title {Template XML file}]
        }
        typevariable _templateRoot {}
        ## The template root.
        typemethod _openTemplateFile {} {
            ## Open a template file.
            
            #      puts stderr "*** $type _openTemplateFile: _templateRoot = $_templateRoot"
            set initfile [$sheetTemplateE get]
            set file [$sheetTemplateDialog draw -parent $sheetTemplateLF \
                      -root   $_templateRoot \
                      -initialfile $initfile]
            #      puts stderr "*** $type _openTemplateFile: file = $file"
            if {"$file" eq ""} {return}
            $sheetTemplateE delete 0 end
            $sheetTemplateE insert end [file tail "$file"]
        }
        typemethod edit {args} {
            ## @publicsection Edit an existing sheet file.
            # @param ... Options:
            # @arg -sheetclass The sheet class to edit.
            
            set sheetclass [from args -sheetclass]
            $type _createEditDialog
            set templateFile [::RolePlayingDB3::Configuration getoption Template]
            set mp [file rootname [file tail $templateFile]]
            vfs::zip::Mount $templateFile $mp
            set t1 [lindex [lsort -dictionary [glob -nocomplain \
                                               [file join $mp $sheetclass \
                                                *.xml]]] 0]
            set _templateRoot [file join $mp $sheetclass]
            $sheetTemplateE delete 0 end
            $sheetTemplateE insert end [file tail "$t1"]
            set answer [$_editDialog draw]
            switch $answer {
                0 {
                    set templateFile [file join $_templateRoot [$sheetTemplateE get]]
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
        method getfile {} {
            ## @privatesectionRetrieve the current file name
            # @returns the file name.
            
            return "$currentFilename"
        }
        typemethod new {args} {
            ## @publicsection Create a new sheet and edit it.
            # @param ... Options:
            # @arg -template Sheet template to use.
            # @arg -sheetclass Sheet class to create.
            # @arg -parent Parent window.
            
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
            ## Open a new sheet file.
            
            set currentFilename {}
            set currentBaseFilename *noname*
            set path [$type genname $options(-sheetclass)]
            set tempfile [file join $::RolePlayingDB3::TmpDir $path]
            while {[file exists $tempfile]} {
                set path [$type genname $options(-sheetclass)]
                set tempfile [file join $::RolePlayingDB3::TmpDir $path]
            }
            vfs::rpg::Mount $tempfile /$path
            file mkdir [file join /$path media]
            close [open [file join /$path media flag] w]
            file mkdir [file join /$path xml]
            close [open [file join /$path xml flag] w]
            [winfo toplevel $win] configure -title "$options(-sheetclass) Edit: $currentBaseFilename"
        }
        method new {} {
            ## Create a new sheet.
            $type new -parent $win -sheetclass $options(-sheetclass) \
                  -template $options(-template)
        }
        typevariable genindex 0
        ## @privatesection Gensym index.
        typemethod genname {class} {
            ## Generate a unique symbol.
            # @param class Symbol class.
            # @returns a new unique symbol.
            
            incr genindex
            return [format {%s%05d} [string toupper $class] $genindex]
        }
        method open {} {
            ## @publicsection Open an existing sheet file.
            
            $type open -parent $win -sheetclass $options(-sheetclass)\
                  -like $currentFilename
        }
        typemethod open {args} {
            ## Open an existing sheet file.
            # @param ... Options:
            # @arg -sheetclass Sheet class to open.
            # @arg -like Filename suggestion.
            # @arg -parent Parent window.
            
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
        typemethod openfile {filename} {
            ## Open an existing sheet file.
            # @param filename File to open.
            
            set inpath [$type genname xmlpeek]
            if {[catch {vfs::zip::Mount $filename $inpath} message]} {
                error "$::argv0: $type: openfile $filename: $filename"
                return
            }
            if {[catch {open [file join $inpath xml sheet.xml] r} shfp]} {
                error "$::argv0: $type: openfile $filename: Illformed sheet bundle: sheet.xml cannot be opened: $shfp"
                return
            }
            set XML [read $shfp]
            close $shfp
            vfs::unmount $inpath
            set fileSheet [ParseXML TopContainer $XML]
            if {[catch {::RolePlayingDB3::SheetClasses validate $fileSheet}]} {
                error "$::argv0: $type: openfile $filename: Not a valid sheet file"
                return
            }
            set sheetclass $fileSheet
            set newTop [RolePlayingDB3::RPGToplevel \
			.[string tolower $sheetclass]%AUTO% \
                        -mainframeconstructor $type \
			-mainframetemplate {} \
			-sheetclass $sheetclass \
			-openfilename $filename \
			-class ${sheetclass}Editor]
        }      
        proc _copyCreateRPGdirs {src dest} {
            ## @privatesection Helper proc to copy files from the Zip file 
            # bundle.
            # @param src Source file/directory
            # @param dest Destination directory.
            
            file mkdir [file join $dest media]
            if {[catch {glob -nocomplain [file join $src media *]} mediafiles]} {
                close [open [file join $dest media flag] w]
            } else {
                foreach f $mediafiles {
                    file copy $f [file join $dest media]
                }
            }
            file mkdir [file join $dest xml]
            if {[catch {glob -nocomplain [file join $src xml *]} xmlfiles]} {
                close [open [file join $dest xml flag] w]
            } else {
                foreach f $xmlfiles {
                    file copy $f [file join $dest xml]
                }
            }
        }
        method openold {_filename} {
            ## @publicsection Open an old file.
            # @param _filename File to open.
            
            set path [$type genname $options(-sheetclass)]
            set tempfile [file join $::RolePlayingDB3::TmpDir $path]
            while {[file exists $tempfile]} {
                set path [$type genname $options(-sheetclass)]
                set tempfile [file join $::RolePlayingDB3::TmpDir $path]
            }
            vfs::rpg::Mount $tempfile /$path
            set currentFilename $_filename 
            set currentBaseFilename [file tail $currentFilename]
            set inpath [$type genname $options(-sheetclass)]
            vfs::zip::Mount $currentFilename $inpath
            _copyCreateRPGdirs $inpath /$path
            vfs::unmount $inpath
            [winfo toplevel $win] configure -title "$options(-sheetclass) $currentBaseFilename"
        }
        method save {} {
            ## Save the sheet to the current file.
            
            $self saveas $currentFilename
        }
        method saveas {{_filename {}}} {
            ## Save the sheet to a  new file.
            # @param _filename File to save to.
            
            if {"$_filename" eq {}} {
                set _filename [tk_getSaveFile -defaultextension .rpg \
                               -filetypes $filetypes \
                               -initialdir [file dirname $currentFilename] \
                               -initialfile $currentFilename \
                               -parent $win \
                               -title "Save As File"]
            }
            if {"$_filename" eq {}} {return}
            #puts stderr "*** $self saveas: isdirty = $isdirty"  
            if {$isdirty} {$self recreateXML}
            #$self recreateXML
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
            ## Print (write to PDF) the current sheet.
            
            set printfile "[file rootname $currentFilename].pdf"
            if {"$printfile" eq ".pdf"} {set printfile "$options(-sheetclass).pdf"}
            set pdfobj [::RolePlayingDB3::PrintDialog drawPrintDialog \
                        -parent $win \
                        -what $options(-sheetclass) \
                        -filename $printfile]
            if {"$pdfobj" eq ""} {return}
            if {"$currentFilename" eq ""} {
                set heading "$currentBaseFilename"
            } else {
                set heading [file tail "$currentFilename"]
            }
            $sheetframe outputXMLToPDF $pdfobj $heading
            ::RolePlayingDB3::PrintDialog printprogress end      
            $pdfobj destroy
        }
        method close {args} {
            ## Close the current sheet.
            # @param ... Options: none
            
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
            ## Constructor -- create a sheet edit frame.
            # @param widgetpath The widget path.
            # @param ... Options:
            # @arg -template Template to use (if this is a new sheet)
            # @arg -sheetclass Sheet class (optional)
            # @arg -openfilename Existing file to open (optional)

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
                set fileSheet [ParseXML TopContainer $XML]
                if {"$fileSheet" ne "$options(-sheetclass)"} {
                    if {[catch {::RolePlayingDB3::SheetClasses validate $fileSheet}]} {
                        error "Not a valid sheet file: $options(-openfilename)"	    
                    }
                    if {"$options(-sheetclass)" ne ""} {tk_messageBox -type ok -icon warning -message "Expected a $options(-sheetclass) file, but got a $fileSheet file."}
                    set options(-sheetclass) $fileSheet
                }
            } else {
                error "Neither -template nor -openfilename was passed!"
            }
            install banner using tk::label $win.banner \
                  -image $bannerImage($options(-sheetclass)) -anchor w \
                  -background $bannerBackgrounds($options(-sheetclass))
            pack $banner -fill x
            install toolbar using ButtonBox $win.toolbar -orient horizontal
            pack $toolbar -fill x 
            $toolbar add tk::button extractmedia -text {Extract Media} \
                  -command [mymethod _extractmedia]
            install sheetframe using ::RolePlayingDB3::XMLContentEditor \
                  $win.sheetframe -xml $XML -isnewobject $isnew \
                  -templatevariable [myvar options(-template)] \
                  -dirtyvariable [myvar isdirty] \
                  -filewidgethandler [mymethod _filewidgethandler] \
                  -xmlfile $xmlfile -basedirectory /$path \
                  -rootcontainer $options(-sheetclass)
            pack $sheetframe -fill both -expand yes
            $self configurelist $args
            #      puts stderr "*** $type $self: parseMode = $parseMode"
            #      if {"$parseMode" eq "file"} {
            #	puts stderr "*** $type $self: options(-template) is:\n$options(-template)"
            #      }
        }
        method _extractmedia {} {
            ## @privatesection Extract a media file.
            
            set sourcefile   [$type draw_getMediaFileDialog \
                              -parent $win -title "File to extract" \
                              -root [file join /$path media] \
                              -saveoropen open \
                              -bannerimage $openMediaFileDialogBanner($options(-sheetclass)) \
                              -bannerbackground $bannerBackgrounds($options(-sheetclass))]
            if {"$sourcefile" eq ""} {return}
            if {[file tail $sourcefile] eq "flag"} {
                tk_messageBox -parent $win -type ok -icon info -message "You cannot extract place holder (flag) files!"
                return
            }
            if {[file system $sourcefile] ne [file system /$path]} {
                tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
                return
            }
            if {![string match [file normalize [file join /$path media]]/* [file normalize $sourcefile]]} {
                tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the media folder!"
                return
            }
            set destfile [tk_getSaveFile -parent $win -title "File to save as" \
                          -initialfile [file tail $sourcefile]]
            if {"$destfile" eq ""} {return}
            file copy -force "$sourcefile" "$destfile"
            tk_messageBox -parent $win -type ok -icon info -message "File [file tail $sourcefile] extracted to $destfile."
        }
        method recreateXML {} {
            ## @publicsection Regenerate XML string file.
            
            $sheetframe recreateXML [file join /$path xml sheet.xml]
        }
        method _filewidgethandler {curfile} {
            ## @privatesection File widget handler.
            # @param curfile current file name
            # @returns a processed (santized) filename.
            
            if {"$curfile" eq ""} {return $curfile}
            if {[file pathtype "$curfile"] eq "relative" &&
                "media" eq [lindex [file split $curfile] 0]} {
                return "$curfile"
            } else {
                file copy -force "$curfile" [file join /$path media [file tail $curfile]]
                return "[file join media [file tail $curfile]]"
            }
        }
    }
}

## @}

package provide RPGSheetEdit 1.0
