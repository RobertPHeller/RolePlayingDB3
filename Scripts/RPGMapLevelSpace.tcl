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

## @addtogroup RPGSupport
# @{


package require vfs::rpg
package require vfs::zip
package require ZipArchive
package require RPGUtilities
package require pdf4tcl
package require ParseXML
package require IconImage
package require Dialog
package require ButtonBox
package require LabelFrames
package require ScrollableFrame
package require Dialog

namespace eval RolePlayingDB3 {
    ##
    snit::macro ::RolePlayingDB3::MapBundleMountPoint {} {
        ## Macro to add the -mapbundlemountpoint option.
        
        option {-mapbundlemountpoint mapBundleMountPoint MapBundleMountPoint} \
              -readonly yes -default /MAP -validatemethod validatemountpoint
        proc isvalidmountpoint {mp} {
            #puts stderr "*** isvalidmountpoint $mp"
            foreach {system additional} [file system $mp] {break}
            #puts stderr "*** isvalidmountpoint:  system = $system, additional = $additional"
            if {"$system" eq "tclvfs" &&
                [string match {::snit::RT.CallInstance ::vfs::rpg::RPGFileSystem::* _handler} "$additional"] } {
                return yes
            } else {
                return no
            }
        }
        method validatemountpoint {option value} {
            #puts stderr "*** $self validatemountpoint $option $value"
            #puts stderr "*** $self validatemountpoint: exists: [file exists $value]"
            #puts stderr "*** $self validatemountpoint: readable: [file readable $value]"
            #puts stderr "*** $self validatemountpoint: writable: [file writable $value]"
            #puts stderr "*** $self validatemountpoint: isdirectory: [file isdirectory $value]"
            #puts stderr "*** $self validatemountpoint: isvalidmountpoint: [isvalidmountpoint $value]"
            if {[file exists $value] && [file readable $value] &&
                [file writable $value] && [file isdirectory $value] &&
                [isvalidmountpoint $value]} {
                return $value
            } else {
                error "Expected a valid tclvfs / vfs::rpg::RPGFileSystem mount point for $option, but got $value"
            }
        }     
    }
    snit::macro ::RolePlayingDB3::MapEditorOption {} {
        ## Macro to add the -mapeditor option.
        
        option {-mapeditor mapEditor MapEditor} \
              -readonly yes -default {} -validatemethod validatemapeditor
        method validatemapeditor {option value} {
            #
            
            #      puts stderr "*** $self validatemapeditor $option $value"
            #      puts stderr "*** $self validatemapeditor: winfo exists $value = [winfo exists $value]"
            #      puts stderr "*** $self validatemapeditor: winfo toplevel $value = [winfo toplevel $value]"
            #      puts stderr "*** $self validatemapeditor: winfo class $value = [winfo class $value]"
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
        ## Macro to add the -leveleditor option
        option {-leveleditor levelEditor LevelEditor} \
              -readonly yes -default {} -validatemethod validatemapeditor
        method validatemapeditor {option value} {
            #
            
            
            #      puts stderr "*** $self validateleveleditor $option $value"
            #      puts stderr "*** $self validateleveleditor: winfo exists $value = [winfo exists $value]"
            #      puts stderr "*** $self validateleveleditor: winfo toplevel $value = [winfo toplevel $value]"
            #      puts stderr "*** $self validateleveleditor: winfo class $value = [winfo class $value]"
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
        ## Macro to add the -leveldir option
        
        option {-leveldir levelDir LevelDir} \
              -readonly yes -default {} -validatemethod validateleveldir
        method validateleveldir {option value} {
            #
            
            
            #      puts stderr "*** [list $self validateleveldir $option $value]"
            if {(([file exists $value] && [file isdirectory $value]) ||
                 ![file exists $value]) &&
                [file tail [file dirname $value]] eq "Levels" &&
                [isvalidmountpoint [file dirname [file dirname $value]]]} {
                return $value
            } else {
                error "Expected a valid level directory for $option, but got $value"
            }
        }	
    }
    snit::macro ::RolePlayingDB3::SpaceFileOption {} {
        ## Macro to add -spacefile option.
        option {-spacefile spaceFile SpaceFile} \
              -readonly yes -default {} -validatemethod validatespacefile
        method validatespacefile {option value} {
            #
            
            #      puts stderr "*** [list $self validatespacefile $option $value]"
            if {![file isdirectory $value] && 
                (([file exists $value] && [file writable $value]) ||
                 [file writable [file dirname $value]]) &&
                [file tail [file dirname [file dirname $value]]] eq "Levels" &&
                [isvalidmountpoint [file dirname \
                                    [file dirname [file dirname $value]]]]} {
                return $value
            } else {
                error "Expected a valid space file for $option, but got $value"
            }
        }	
    }
    
    snit::widget MapEditor {
        ## Map editor widget.
        option -template -readonly yes -default {}
        ::RolePlayingDB3::OpenFilename
        typevariable defaultfilename "map.rpg"
        ## @privatesection Default filename.
        variable currentFilename
        ## Current filename.
        variable currentBaseFilename
        ## Current base filename.
        variable isdirty no
        ## Dirty flag.
        method setdirty {} {
            ## Set dirty flag
            set isdirty yes
        }
        variable isdirtylevel no
        ## Dirty level flag
        method setdirtylevel {} {
            ## Set dirty level flag
            set isdirtylevel yes
        }
        variable levelEditors [list]
        ## Open Level editors.
        variable path
        ## Mount point.
        variable tempfile
        ## Backing file.
        variable needmediatreeupdated no
        ## Media update flag.
        
        component banner
        ## Banner.
        component toolbar
        ## Toolbar.
        component panes
        ## Panes.
        component   sbpane
        ## Sidebar pane.
        component     sbpanes
        ## Sidebar panes.
        component       levelpane
        ## Level list pane.
        component	      leveltree
        ## Level tree.
        component       mediapane
        ## Media tree pane.
        component	      mediatree
        ## Media tree.
        component   mappane
        ## Map pane.
        component     mapframe
        ## Map frame.
        
        typevariable filetypes {
            {{Map Files} {.rpg}             }
            {{All Files}        *           }
        }
        ## File types.
        typemethod myfiletypes {} {
            ## Fetch file types.
            # @returns file types structuure.
            
            return $filetypes
        }
        typecomponent _editDialog
        ## Edit dialog.
        typecomponent _chooseLevelDialog
        ## Choose level dialog.
        typecomponent _chooseMediaFolderDialog
        ## Choose Media folder dialog.
        typecomponent _getMediaFileDialog
        ## Choose Media file dialog.
        typevariable bannerImage
        ## Banner image.
        typevariable newLevelDialogBanner
        ## New level dialog banner.
        typevariable oldLevelDialogBanner
        ## Old level dialog banner.
        typevariable newMediaFolderDialogBanner
        ## New media folder dialog banner.
        typevariable oldMediaFolderDialogBanner
        ## Old media folder dialog banner.
        typevariable openMediaFileDialogBanner
        ## Open media file dialog banner.
        typevariable saveMediaFileDialogBanner
        ## Save media file dialog banner.
        typevariable dialogIcon
        ## Dialog icon
        typemethod   getDialogIcon {} {
            ## Fetch dialog icon.
            # @returns the dialog icon.
            
            return $dialogIcon
        }
        typevariable bannerBackground #a2de86
        ## Banner background color.
        typevariable mapTemplateXML {<?xml version="1.0" ?>
            <rpgv3:Map xmlns:rpgv3="http://www.deepsoft.com/roleplayingdb/v3xmlns" 
            name="Map" >Global Map Information
            <rpgv3:Field name="Name" type="Word / Short Phrase" updatable="yes" />
            <rpgv3:Field name="Campaign" type="Word / Short Phrase" updatable="yes" />
            <rpgv3:Field name="Game Master" type="Word / Short Phrase" updatable="yes" />
            <rpgv3:Field name="Space Shape" type="Enumerated Type" values="Square Hexigonal" id="spaceshape" updatable="no" />
            <rpgv3:Field name="Short Description" type="Long Text" updatable="yes" />
            <rpgv3:Field name="Long Description" type="Document" updatable="yes" />
            </rpgv3:Map>
        }
        ## Empty default map.
        method spaceshape {} {
            ## Return the space shape.
            # @returns space shape.
            
            return [[$mapframe getElementWidgetById spaceshape] cget -text]
        }
        typevariable hWidth 0.5
        ## Half width.
        typevariable halfsl .288675
        ## Half hex side length.
        typevariable squareCoords {-.5 -.5 .5 .5}
        ## Square coords.
        typevariable hexCoords {-.5  .288675
            -.5 -.288675 
            0.0 -.57735 
            .5 -.288675 
            .5  .288675 
            0.0  .57735 
            -.5  .288675}
        ## Hex coords.
        typevariable hexXoffset  .5
        ## Hex X offset.
        typevariable hexSideLength   .57735
        ## Hex side length.
        typevariable hexPeakHeight   .288675
        ## Hex  peak height.
        method scaleXY {X Y scale} {
            ## @publicsection Scale space on map.
            # @param X X coord.
            # @param Y Y coord.
            # @param scale Scale factor.
            # @returns Coordlist, scaled and tranlated.
            
            #      puts stderr "*** $self scaleXY $X $Y $scale"
            switch [$self spaceshape] {
                Square {
                    set result [list [expr {$X * $scale}] [expr {$Y * $scale}]]
                }
                Hexigonal {
                    if {(int($Y) % 2) == 1} {
                        set X1 [expr {$X + $hexXoffset}]
                    } else {
                        set X1 $X
                    }
                    set Y1 [expr {$Y * ($hexSideLength + $hexPeakHeight)}]
                    set result [list [expr {$X1 * $scale}] [expr {$Y1 * $scale}]]
                }
            }
            #      puts stderr "*** $self scaleXY: returning $result"
            return $result
        }
        method unscaleXY {cX cY scale} {
            ## Unscale space on map.
            # @param cX X canvas coord.
            # @param cY Y canvas coord.
            # @param scale Scale factor.
            # @returns Coord list, unscaled and translated.
            
            set scale [expr {double($scale)}]
            switch [$self spaceshape] {
                Square {
                    return [list [expr {int($cX / $scale)}] [expr {int($cY / $scale)}]]
                }
                Hexigonal {
                    set Y1 [expr {$cY / $scale}]
                    set X1 [expr {$cX / $scale}]
                    set Y  [expr {int($Y1 / ($hexSideLength + $hexPeakHeight))}]
                    if {(int($Y) % 2) == 1} {
                        set X [expr {int($X1 - $hexXoffset)}]
                    } else {
                        set X [expr {int($X1)}]
                    }
                    return [list [expr {int($X)}] [expr {int($Y)}]]
                }
            }
        }
        method drawspace {canvas X Y color size args} {
            ## Draw a space.
            # @param canvas The canvas to draw on.
            # @param X X coord.
            # @param Y Y coord.
            # @param color The color of the space.
            # @param size The size of the space.
            # @param ... Additional args passed to canvas create.
            
            switch [$self spaceshape] {
                Square {
                    set index [$canvas create rectangle $squareCoords \
                                     -fill $color -outline black {*}$args]
                }
                Hexigonal {
                    set index [$canvas create polygon $hexCoords \
                                     -fill $color -outline black {*}$args]
                }
            }
            $canvas scale $index 0 0 $size $size
            $canvas move $index $X $Y
        }
        ::RolePlayingDB3::GeneratePrintDialog map {printLevelsLCB} {
            set printLevelsLCB [LabelComboBox $frame.printLevelsLCB \
                                -label "Print Levels?" -labelwidth 12 \
                                -editable no \
                                -values {yes no}]
            pack $printLevelsLCB -fill x
            $printLevelsLCB set [lindex [$printLevelsLCB cget -values] 0]
        }
        typeconstructor {
            ## @privatesection Type constructor -- one time initialization.
            
            set bannerImage [image create photo \
                             -file [file join $::RolePlayingDB3::ImageDir \
                                    MapBanner.png]]
            set dialogIcon [image create photo \
                            -file [file join $::RolePlayingDB3::ImageDir \
                                   MapDialogIcon.png]]
            set printerIcon [image create photo \
                             -file [file join $::RolePlayingDB3::ImageDir \
                                    largePrinter.gif]]
            set newLevelDialogBanner [image create photo \
                                      -file [file join $::RolePlayingDB3::ImageDir \
                                             CreateLevelBanner.png]]
            set oldLevelDialogBanner [image create photo \
                                      -file [file join $::RolePlayingDB3::ImageDir \
                                             SelectLevelBanner.png]]
            set newMediaFolderDialogBanner [image create photo \
                                            -file [file join $::RolePlayingDB3::ImageDir \
                                                   CreateNewMediaFolderBanner.png]]
            set oldMediaFolderDialogBanner [image create photo \
                                            -file [file join $::RolePlayingDB3::ImageDir \
                                                   SelectMediaFolderBanner.png]]
            set openMediaFileDialogBanner [image create photo \
                                           -file [file join $::RolePlayingDB3::ImageDir \
                                                  OpenMediaFileBanner.png]]
            set saveMediaFileDialogBanner [image create photo \
                                           -file [file join $::RolePlayingDB3::ImageDir \
                                                  SaveMediaFileBanner.png]]
            set _printdialog {}
            set _editDialog {}
            set _chooseLevelDialog {}
            set _chooseMediaFolderDialog {}
            set _getMediaFileDialog {}
        }
        
        typemethod _createEditDialog {} {
            ## Create edit dialog.
            
            if {"$_editDialog" ne "" && [winfo exists "$_editDialog"]} {return}
            set _editDialog [Dialog .editMapEditorDialog -image $dialogIcon \
                             -cancel 2 -default 0 -modal local \
                             -parent . -side bottom \
                             -title "Edit Map" \
                             -transient yes]
            $_editDialog add new    -text {Create}
            $_editDialog add open   -text {Open}  
            $_editDialog add cancel -text {Cancel}
            pack [message [$_editDialog getframe].message \
                  -text "Create a new Map file or\nopen an existing Map file?" \
                  -aspect 500] -fill both
        }
        typemethod edit {args} {
            ## @publicsection Edit an existing map.
            # @param ... Options: None.
            
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
        typemethod createChooseLevelDialog {} {
            ## @privatesection Create choose level dialog.
            
            if {"$_chooseLevelDialog" ne "" && [winfo exists "$_chooseLevelDialog"]} {return}
            set _chooseLevelDialog [::RolePlayingDB3::chroot_chooseDirectory \
                                    .chooseLevelDialog \
                                    -bannerimage $newLevelDialogBanner \
                                    -bannerbackground $bannerBackground]
        }
        typemethod draw_chooseLevelDialog {args} {
            ## Draw the choose level dialog
            # @param ... Options: passed to the dialog draw method.
            
            $type createChooseLevelDialog
            return [$_chooseLevelDialog draw {*}$args]
        }
        typemethod createChooseMediaFolderDialog {} {
            ## Create the choose media folder dialog.
            
            if {"$_chooseMediaFolderDialog" ne "" && [winfo exists "$_chooseMediaFolderDialog"]} {return}
            set _chooseMediaFolderDialog [::RolePlayingDB3::chroot_chooseDirectory \
                                          .chooseMediaFolderDialog \
                                          -bannerimage $newMediaFolderDialogBanner \
                                          -bannerbackground $bannerBackground]
        }
        typemethod draw_chooseMediaFolderDialog {args} {
            ## Draw the choose media folder dialog.
            # @param ... Options: passed to the dialog draw method.
            
            $type createChooseMediaFolderDialog
            return [$_chooseMediaFolderDialog draw {*}$args]
        }
        typemethod createGetMediaFileDialog {} {
            ## Create the get media file dialog.
            
            if {"$_getMediaFileDialog" ne "" && [winfo exists "$_getMediaFileDialog"]} {return}
            set _getMediaFileDialog [::RolePlayingDB3::chroot_getFile \
                                     .getMediaFileDialog \
                                     -bannerimage $saveMediaFileDialogBanner \
                                     -bannerbackground $bannerBackground]
        }
        typemethod draw_getMediaFileDialog {args} {
            ## Draw the get media file dialog
            # @param ... Options: passed to the dialog draw method
            
            $type createGetMediaFileDialog
            return [$_getMediaFileDialog draw {*}$args]
        }
        method getfile {} {
            ## Fetch the current filename 
            # @returns the file name.
            
            return "$currentFilename"
        }
        typemethod new {args} {
            ## @publicsection Create a new map.
            # @param ... Options:
            # @arg -template Template to use.
            # @arg -parent The parent window.
            
            set templateXML [from args -template $mapTemplateXML]
            set parent [from args -parent .]
            
            set newTop [RolePlayingDB3::RPGToplevel \
                        .map%AUTO% \
                        -mainframeconstructor $type \
                        -mainframetemplate $templateXML \
                        -class MapEditor]
        }
        method opennew {} {
            ## Create a new map.
            
            set currentFilename {}
            set currentBaseFilename *noname*
            set path [$type genname]
            set tempfile [file join $::RolePlayingDB3::TmpDir $path]
            while {[file exists $tempfile]} {
                set path [$type genname]
                set tempfile [file join $::RolePlayingDB3::TmpDir $path]
            }
            vfs::rpg::Mount $tempfile /$path
            file mkdir [file join /$path Levels]
            close [open [file join /$path Levels flag] w]
            file mkdir [file join /$path media]
            close [open [file join /$path media flag] w]
            file mkdir [file join /$path xml]
            close [open [file join /$path xml flag] w]
            [winfo toplevel $win] configure -title "Map Edit: $currentBaseFilename"
        }
        method new {} {
            ## Create a new map.
            
            $type new -parent $win -template $mapTemplateXML
        }
        typevariable genindex 0
        ## @privatesection Symbol generation index.
        typemethod genname {} {
            ## Generate a new unique symbol.
            # @returns a new unique symbol.
            
            incr genindex
            return [format {MAP%05d} $genindex]
        }
        typemethod open {args} {
            ## @publicsection Open an existing map bundle file.
            # @param ... Options:
            # @arg -parent Parent window.
            # @arg -like Open a file like this one.
            
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
        typemethod openfile {filename} {
            ## Open an existing map file.
            # @param filename File to open.
            
            set currentFilename "$filename"
            if {"$currentFilename" eq ""} {return}
            set newTop [RolePlayingDB3::RPGToplevel \
			.map%AUTO% \
			-mainframeconstructor $type \
			-mainframetemplate {} \
			-openfilename $currentFilename \
			-class MapEditor]
        }
        proc _copyCreateRPGdirsHelper {srcfile destdir} {
            ## @privatesection Copy RPG dirs
            # @param srcfile Source file or diectory.
            # @param destdir Destination directory.
            
            #puts stderr "*** _copyCreateRPGdirsHelper $srcfile $destdir"
            file stat $srcfile fssrc
            #puts stderr "*** _copyCreateRPGdirsHelper: $srcfile $fssrc(size) bytes, type: $fssrc(type)"
            set filename [file tail $srcfile]
            if {$fssrc(type) ne "directory"} {
                file copy $srcfile $destdir
                file stat [file join $destdir $filename] fs
                #puts stderr "*** _copyCreateRPGdirsHelper: [file join $destdir $filename]: $fs(size) bytes"
            } else {
                set files [glob -nocomplain [file join $srcfile *]]
                file mkdir [file join $destdir $filename]
                foreach f $files {
                    _copyCreateRPGdirsHelper $f [file join $destdir $filename]
                }
            }
        }
        proc _copyCreateRPGdirs {src dest} {
            ## Copy a map file bundle to a temp file system.
            # @param src Source bundle.
            # @param dest Destination temp file system.
            
            file mkdir [file join $dest Levels]
            if {[catch {glob -nocomplain [file join $src Levels *]} levelfiles]} {
                close [open [file join $dest Levels flag] w]
            } else {
                foreach f $levelfiles {
                    _copyCreateRPGdirsHelper $f [file join $dest Levels]
                }
            }
            file mkdir [file join $dest media]
            if {[catch {glob -nocomplain [file join $src media *]} mediafiles]} {
                close [open [file join $dest media flag] w]
            } else {
                foreach f $mediafiles {
                    _copyCreateRPGdirsHelper $f [file join $dest media]
                }
            }
            file mkdir [file join $dest xml]
            if {[catch {glob -nocomplain [file join $src xml *]} xmlfiles]} {
                close [open [file join $dest xml flag] w]
            } else {
                foreach f $xmlfiles {
                    _copyCreateRPGdirsHelper $f [file join $dest xml]
                }
            }
        }
        method openold {_filename} {
            ## @publicsection Open an old file.
            # @param _filename Filename to open.
            
            set path [$type genname]
            set tempfile [file join $::RolePlayingDB3::TmpDir $path]
            while {[file exists $tempfile]} {
                set path [$type genname]
                set tempfile [file join $::RolePlayingDB3::TmpDir $path]
            }
            vfs::rpg::Mount $tempfile /$path
            set currentFilename $_filename 
            set currentBaseFilename [file tail $currentFilename]
            set inpath [$type genname]
            vfs::zip::Mount $currentFilename $inpath
            _copyCreateRPGdirs $inpath /$path
            vfs::unmount $inpath
            [winfo toplevel $win] configure -title "Map Editor: $currentBaseFilename"
        }
        method save {} {
            ## Save the current file.
            
            $self saveas $currentFilename
        }
        method saveas {{_filename {}}} {
            ## Save the bundle to the speified file.
            # @param _filename Filename to save to.
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
                if {[$le checksave] eq "error"} {return}
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
            ## Print (to a PDF file) the current map.
            
            set printfile "[file rootname $currentFilename].pdf"
            if {"$printfile" eq ".pdf"} {set printfile "Map.pdf"}
            set pdfobj [$type drawPrintDialog \
                        -parent $win \
                        -what Map -filename $printfile]
            if {"$pdfobj" eq ""} {return}
            if {"$currentFilename" eq ""} {
                set heading [file tail "$currentBaseFilename"]
            } else {
                set heading [file tail "$currentFilename"]
            }
            $mapframe outputXMLToPDF $pdfobj "$heading: map info"
            set curpage [$mapframe getpageno]
            set curline [$mapframe getlineno]
            if {[$printLevelsLCB cget -text]} {
                foreach l [lsort -command [mymethod _sortlevelsbydepth] \
                           [glob -nocomplain -type d \
                            [file join /$path Levels *]]] {
                    ::RolePlayingDB3::LevelEditor printLevel $pdfobj $l curpage curline \
                      "$heading: level info for [file tail $l]" \
                      -mapbundlemountpoint /$path -parent $win \
                      -mapeditor [winfo toplevel $win]
                }
            }
            ::RolePlayingDB3::PrintDialog printprogress end      
            $pdfobj destroy
        }
        method close {args} {
            ## Close the current file.
            # @param ... Options: None.
            
            set dontsave no
            foreach le $levelEditors {
                if {[$le getdirty]} {
                    set isdirtylevel yes
                    break
                }
            }
            if {$isdirty || $isdirtylevel} {
                set ans [tk_messageBox -type yesnocancel -icon question -parent $win \
                         -message "Save data before closing window?"]
                switch $ans {
                    yes {$self save}
                    cancel {return}
                    no {set dontsave yes}
                }
            }
            foreach le $levelEditors {
                catch {$le close -dontask yes -dontsave $yes}
            }
            vfs::unmount /$path
            file delete $tempfile
            destroy [winfo toplevel $win]
        }
        constructor {args} {
            ## Constructor -- create a map edit widget.
            # @param widgetpath Widget path.
            # @param ... Options:
            # @arg -template Alternitive emplate to use.
            # @arg -openfilename File to open.
            
            $self configurelist $args
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
            install banner using ttk::label $win.banner \
                  -image $bannerImage -anchor w \
                  -background $bannerBackground
            pack $banner -fill x
            install toolbar using ButtonBox $win.toolbar -orient horizontal
            pack $toolbar -fill x
            $toolbar add ttk::button newlevel -text {New Level} \
                  -command [mymethod _newlevel]
            $toolbar add ttk::button dellevel -text {Delete Level} \
                  -command [mymethod _deletelevel]
            $toolbar add ttk::button editlevel -text {Edit Level} \
                  -command [mymethod _editlevel]
            $toolbar add ttk::button extractmedia -text {Extract Media} \
                  -command [mymethod _extractmedia]
            install panes using ttk::panedwindow $win.panes -orient horizontal
            pack $panes -fill both -expand yes
            install sbpanes using ttk::panedwindow $panes.sbpanes -orient vertical
            $panes add $sbpanes -weight 1
            install leveltree using ::RolePlayingDB3::LabeledDirTree \
                  $sbpanes.leveltree -showextension no -filepattern *.xml \
                  -label "Levels" -nofiles yes -opendirs no \
                  -sortfunction [mymethod _sortlevelsbydepth]
            $sbpanes add $leveltree -weight 1
            install mediatree using ::RolePlayingDB3::LabeledDirTree \
                  $sbpanes.mediatree -showextension yes -filepattern * \
                  -label "Media"
            $sbpanes add $mediatree -weight 5
            
            install mapframe using ::RolePlayingDB3::XMLContentEditor \
                  $panes.mapframe -xml $XML -isnewobject $isnew \
                  -rootcontainer Map \
                  -dirtyvariable [myvar isdirty] \
                  -filewidgethandler [mymethod _filewidgethandler] \
                  -xmlfile $xmlfile -basedirectory /$path
            $panes add $mapframe -weight 5
            $leveltree configure -directory [file join /$path Levels]
            $mediatree configure -directory [file join /$path media]
        }
        method _sortlevelsbydepth {la lb} {
            ## @privatesection Sort levels by depth.
            # @param la Level a.
            # @param lb  Levelb.
            # @returns the level difference between a and b.
            
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
            set a_depth [::RolePlayingDB3::XMLContentEditor ExtractTagValue $xmla Depth 0]
            #      puts stderr "*** $self _sortlevelsbydepth: a_depth = $a_depth"
            set b_depth [::RolePlayingDB3::XMLContentEditor ExtractTagValue $xmlb Depth 0]
            #      puts stderr "*** $self _sortlevelsbydepth: b_depth = $b_depth"
            set comp [expr {$a_depth - $b_depth}]
            #      puts stderr "*** $self _sortlevelsbydepth: comp = $comp"
            if {$comp == 0} {
                return [string compare -nocase [file tail $la] [file tail $lb]]
            } else {
                return $comp
            }
        }
        method recreateXML {} {
            ## @publicsection Regenerate the XML string.
            
            set needmediatreeupdated no
            $mapframe recreateXML [file join /$path xml map.xml]
            if {$needmediatreeupdated} {$self updatemediatree}
        }
        method updateleveltree {} {
            ## Update the level tree.
            
            $leveltree redrawdirtree
        }
        method updatemediatree {} {
            ## Update the media tree.
            
            $mediatree redrawdirtree
        }
        method _filewidgethandler {curfile} {
            ## @privatesection Handler for file entries.
            # @param curfile New file to handle.
            
            if {"$curfile" eq ""} {return $curfile}
            if {[file pathtype "$curfile"] eq "relative" &&
                "media" eq [lindex [file split $curfile] 0]} {
                return "$curfile"
            } else {
                file copy -force "$curfile" [file join /$path media [file tail $curfile]]
                set needmediatreeupdated yes
                return "[file join media [file tail $curfile]]"
            }
        }
        method _newlevel {args} {
            ## Create new level.
            # @param ... Options:
            # @arg -parent Parent window.
            
            set parent [from args -parent $win]
            set leveldir [$type draw_chooseLevelDialog \
                          -bannerimage $newLevelDialogBanner \
                          -root [file join /$path Levels] \
                          -mustexist no \
                          -initialdir "New Level" \
                          -parent $win \
                          -title "Level to create"]
            #      puts stderr "*** $self _newlevel: leveldir = $leveldir"
            if {"$leveldir" eq ""} {return ""}
            if {[file system [file dirname $leveldir]] ne [file system /$path]} {
                tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
                return
            }
            if {![string match [file normalize [file join /$path Levels]]/* [file normalize $leveldir]]} {
                tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the Levels folder!"
                return
            }
            if {[file normalize "$leveldir"] eq [file normalize [file join /$path Levels]]} {
                tk_messageBox -parent $win -type ok -icon info -message "You cannot use the Levels folder itself as a level!"
                return
            }
            if {[file exists "$leveldir"]} {
                tk_messageBox -parent $win -type ok -icon info -message "Level [file tail $leveldir] already exists!"
                return
            }
            
            set newleveleditor [::RolePlayingDB3::LevelEditor new \
                                -mapbundlemountpoint /$path \
                                -parent $parent \
                                -leveldir $leveldir \
                                -mapeditor [winfo toplevel $win]]
            if {"$newleveleditor" ne ""} {
                set isdirty yes
                $self updateleveltree
                $self updatemediatree
                lappend levelEditors $newleveleditor
            }
        }
        method _deletelevel {} {
            ## Delete a level.
            
            if {[llength [glob -nocomplain -type d [file join /$path Levels *]]] == 0} {return}
            set selection [$leveltree selection]
            if {[llength $selection] > 0} {
                set level [$leveltree itemcget [lindex $selection 0] -fullpath]
            } else {
                set level [$type draw_chooseLevelDialog \
                           -bannerimage $oldLevelDialogBanner \
                           -root [file join /$path Levels] \
                           -mustexist yes \
                           -parent $win \
                           -title "Level to delete"]
            }
            if {"$level" eq ""} {return}
            if {[file system [file dirname $level]] ne [file system /$path]} {
                tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
                return
            }
            if {![string match [file normalize [file join /$path Levels]]/* [file normalize $level]]} {
                tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the Levels folder!"
                return
            }
            if {[file normalize "$level"] eq [file normalize [file join /$path Levels]]} {
                tk_messageBox -parent $win -type ok -icon info -message "You cannot delete the Levels folder itself!"
                return
            }
            set lelist $levelEditors
            foreach le $levelEditors {
                if {[file normalize "$level"] eq [file normalize "[$le getleveldir]"]} {
                    $le close -dontsave yes
                    set leindex [lsearch -exact $lelist $le]
                    if {$leindex >= 0} {set lelist [lreplace $lelist $leindex $leindex]}
                }
            }
            set levelEditors $lelist
            deletetree $level
            $self updateleveltree
            deletetree [file join /$path media [file tail $level]]
            $self updatemediatree
            set isdirty yes
        }
        method _editlevel {args} {
            ## Edit a level.
            # @param ... Options:
            # @arg -parent Parent window.
            
            set parent [from args -parent $win]
            if {[llength [glob -nocomplain -type d [file join /$path Levels *]]] == 0} {return}
            if {[llength [glob -nocomplain -type d [file join /$path Levels *]]] == 0} {return}
            set selection [$leveltree selection]
            if {[llength $selection] > 0} {
                set level [$leveltree itemcget [lindex $selection 0] -fullpath]
            } else {
                set level [$type draw_chooseLevelDialog \
                           -bannerimage $oldLevelDialogBanner \
                           -root [file join /$path Levels] \
                           -mustexist yes \
                           -parent $win \
                           -title "Level to edit"]
            }
            if {"$level" eq ""} {return}
            #      puts stderr "*** $self _editlevel: file system [file dirname $level] = '[file system [file dirname $level]]', file system /$path = '[file system /$path]'"
            if {[file system [file dirname $level]] ne [file system /$path]} {
                tk_messageBox -parent $parent -type ok -icon info -message "Opps, you stepped off the internal file system!"
                return
            }
            #      puts stderr "[list *** $self _editlevel:  [file normalize [file join /$path Levels]]/* [file normalize $level]]"
            if {![string match [file normalize [file join /$path Levels]]/* [file normalize $level]]} {
                tk_messageBox -parent $parent -type ok -icon info -message "Opps, you stepped out of the Levels folder!"
                return
            }
            #      puts stderr "[list *** $self _editlevel: [file normalize [file join /$path Levels]]]"
            if {[file normalize "$level"] eq [file normalize [file join /$path Levels]]} {
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
                                -mapeditor [winfo toplevel $win] \
                                -parent $parent]
            if {"$newleveleditor" ne ""} {
                lappend levelEditors $newleveleditor
            }
        }
        method removeeditor {le} {
            ## Remove editor.
            # @param le Level editor to remove.
            set leindex [lsearch -exact $levelEditors $le]
            if {$leindex >= 0} {
                set levelEditors [lreplace $levelEditors $leindex $leindex]
            }
        }
        method _extractmedia {} {
            ## Extract media.
            
            set selection [$mediatree selection]
            if {[llength $selection] > 0} {
                set sourcefile [$mediatree itemcget [lindex $selection 0] -fullpath]
                if {![file isfile $destfile]} {
                    set sourcefile   [$type draw_getMediaFileDialog \
                                      -parent $win -title "File to extract" \
                                      -root [file join /$path media] \
                                      -saveoropen open \
                                      -bannerimage $openMediaFileDialogBanner]
                }
            } else {
                set sourcefile   [$type draw_getMediaFileDialog \
                                  -parent $win -title "File to extract" \
                                  -root [file join /$path media] \
                                  -saveoropen open \
                                  -bannerimage $openMediaFileDialogBanner]
            }
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
        proc deletetree {dir} {
            ## Delete a directory tree.
            # @param dir The directory to delete.
            
            foreach d [glob -nocomplain -type d [file join $dir *]] {
                deletetree $d
            }
            foreach f [glob -nocomplain [file join $dir *]] {
                file delete $f
            }
            file delete $dir
        }
    }
    snit::widget LevelEditor {
        ## Level editor
        
        option -template -readonly yes -default {}
        ::RolePlayingDB3::MapBundleMountPoint
        ::RolePlayingDB3::MapEditorOption
        ::RolePlayingDB3::LevelDirOption
        variable currentLevelDir
        ## @privatesection Current level directory.
        method getleveldir {} {
            ## Fetch the current level directory.
            # @returns the current level directory.
            
            return $currentLevelDir
        }
        variable isdirty no
        ## Dirty flag.
        variable needmediatreeupdated
        ## Media update flag.
        variable spaceEditors [list]
        ## List of open space editors.
        variable firstsave yes
        ## First time save flag.
        variable zoommenu
        ## Zoom menu.
        variable zoomfactor 1
        ## Zoom factor.
        variable oldscalefactor 1
        ## Old scale factor.
        variable mymediadir
        ## Media directory.
        variable mymediadirRelative
        ## Relative media directory.
        method setdirty {} {
            ## Set dirty flag.
            
            set isdirty yes
        }
        method getdirty {} {
            ## Get the dirty flag.
            # @returns the dirty flag.
            
            return $isdirty
        }
        variable isdirtyspace no
        ## Dirty space flag.
        method setdirtyspace {} {
            ## Set the dirty space flag.
            
            set isdirtyspace yes
        }
        method checksave {} {
            ## @publicsection Check if save is needed.
            
            foreach se $spaceEditors {
                if {[$se checksave] eq "error"} {return error}
            }
            if {$isdirty} {
                $self recreateXML
                if {$firstsave} {
                    $options(-mapeditor) updateleveltree
                    set firstsave no
                }
                $options(-mapeditor) setdirtylevel
            }
            set isdirty no
        }
        
        component banner
        ## @privatesection Banner.
        component toolbar
        ## Toolbar.
        component panes  
        ## Panes.
        component   sbpane
        ## Sidebar pane.
        component     spacetree
        ## Space tree.
        component   levelpane
        ## Level pane.
        component     levelframe
        ## Level frame.
        
        typevariable bannerImage
        ## Banner image.
        typevariable newSpaceFileDialogBanner
        ## New space file dialog banner.
        typevariable oldSpaceFileDialogBanner
        ## Old space file dialog banner.
        typevariable bannerBackground #a2de86
        ## Banner background color.
        typevariable _getSpaceFileDialog
        ## Get space file dialog.
        typevariable levelTemplateXML {<?xml version="1.0" ?>
            <rpgv3:Level xmlns:rpgv3="http://www.deepsoft.com/roleplayingdb/v3xmlns"
            name="Level" >Level Information
            <rpgv3:Field name="Title" type="Word / Short Phrase" updatable="yes" />
            <rpgv3:Field name="Depth" type="Whole Number" updatable="no" range="-50 50 1" />
            <rpgv3:Field name="Description" type="Long Text" updatable="yes" />
            <rpgv3:LevelMap name="Level Map">Level Map
            <rpgv3:Canvas side="left" id="map" background="gray" />
            <rpgv3:Buttonbox side="right" orient="vertical" id="zoombuttons">
            <rpgv3:Bi label="Zoom In"  id="zoomin" />
            <rpgv3:Bi label="Zoom &gt;"  id="zoomto" />
            <rpgv3:Bi label="Zoom Out" id="zoomout" />
            </rpgv3:Buttonbox >
            </rpgv3:LevelMap >  
            </rpgv3:Level>
        }
        ## Level template XML.
        ::RolePlayingDB3::GeneratePrintDialog level {printSpacesLCB} {
            set printSpacesLCB [LabelComboBox $frame.printSpacesLCB \
				-label "Print Spaces?" -labelwidth 12 \
				-editable no \
				-values {yes no}]
            pack $printSpacesLCB -fill x
            $printSpacesLCB set [lindex [$printSpacesLCB cget -values] 0]
        }
        typeconstructor {
            ## Typeconstructor: one time initialization.
            
            set bannerImage [image create photo \
                             -file [file join $::RolePlayingDB3::ImageDir \
                                    LevelBanner.png]]
            set printerIcon [image create photo \
                             -file [file join $::RolePlayingDB3::ImageDir \
                                    largePrinter.gif]]
            set newSpaceFileDialogBanner [image create photo \
                                          -file [file join $::RolePlayingDB3::ImageDir \
                                                 CreateNewSpaceBanner.png]]
            set oldSpaceFileDialogBanner [image create photo \
                                          -file [file join $::RolePlayingDB3::ImageDir \
                                                 SelectSpaceBanner.png]]
            set _printdialog {}
            set _getSpaceFileDialog {}
        }
        typemethod createGetSpaceFileDialog {} {
            ## Create the get space file dialog.
            
            if {"$_getSpaceFileDialog" ne "" && [winfo exists "$_getSpaceFileDialog"]} {return}
            set _getSpaceFileDialog [::RolePlayingDB3::chroot_getFile \
                                     .getSpaceFileDialog \
                                     -bannerimage $oldSpaceFileDialogBanner \
                                     -bannerbackground $bannerBackground]
        }
        typemethod draw_getSpaceFileDialog {args} {
            ## Draw the get space file dialog.
            # @param ... Options: passed to the draw method of the get space 
            # file dialog.
            $type createGetSpaceFileDialog
            return [$_getSpaceFileDialog draw {*}$args]
        }
        method new {} {
            ## @publicsection Create a new level.
            
            $options(-mapeditor) _newlevel -parent $win
        }
        typemethod new {args} {
            ## Create a new level.
            # @param ... Options:
            # @arg -mapbundlemountpoint Map mount point bundle.
            # @arg -parent Parent window.
            # @arg -mapeditor Parent map editor.
            # @arg -leveldir Level directory.
            # @returns the new toplevel.
            
            set mapbundlemountpoint [from args -mapbundlemountpoint]
            set parent [from args -parent .]
            set mapeditor [from args -mapeditor]
            set leveldir [from args -leveldir]
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
            ## Open a level file.
            
            $options(-mapeditor) _editlevel -parent $win
        }
        typemethod open {args} {
            ## Open a level file.
            # @param ... Options:
            # @arg -mapbundlemountpoint Map mount point bundle.
            # @arg -parent Parent window.
            # @arg -mapeditor Parent map editor.
            # @arg -leveldir Level directory.
            # @returns the new toplevel.
            
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
            ## Save the current file.
            
            $options(-mapeditor) save
        }
        method saveas {{_filename {}}} {
            ## Save to a new file.
            # @param _filename The filename to save to.
            
            $options(-mapeditor) saveas $_filename
        }
        typemethod printLevel {pdfobj leveldir curpageV curlineV heading args} {
            ## Print (to a PDF file) the level.
            # @param pdfobj The PDF object to print to.
            # @param leveldir The level directory.
            # @param curpageV Current page variable.
            # @param curlineV Current line variable.
            # @param heading Heading.
            # @param ... Options:
            # @arg -mapbundlemountpoint The map bundle mount point.
            # @arg -mapeditor The parent map editor.
            # @arg -parent The parent window.
            
            upvar $curpageV curpage
            upvar $curlineV curline
            set mapbundlemountpoint [from args -mapbundlemountpoint]
            set mapeditor           [from args -mapeditor]
            set parent              [from args -parent]
            if {[catch {open [file join $leveldir levelinfo.xml] r} shfp]} {
                error "Illformed map bundle: [file join $currentLevelDir levelinfo.xml] cannot be opened: $shfp"
            }
            set XML [read $shfp]
            close $shfp
            set temppath [::RolePlayingDB3::XMLContentEditor $parent.templevel%AUTO% \
                          -xml $XML -isnewobject no -dirtyvariable {} \
                          -filewidgethandler {}  -xmlfile {} \
                          -basedirectory $mapbundlemountpoint \
                          -rootcontainer Level]
            set map [$temppath getElementWidgetById map]
            foreach s [lsort -dictionary [glob -nocomplain [file join $leveldir *.xml]]] {
                if {[file tail $s] eq "levelinfo.xml"} {continue}
                drawonespace_forprint $map $s $mapeditor
            }
            $temppath outputXMLToPDF $pdfobj $heading $curpage $curline
            set curpage [$temppath getpageno]
            set curline [$temppath getlineno]
            foreach s [lsort -dictionary [glob -nocomplain [file join $leveldir *.xml]]] {
                if {[file tail $s] eq "levelinfo.xml"} {continue}
                #	puts stderr "*** $type printLevel (before printSpace): curpage = $curpage, curline = $curline"
                ::RolePlayingDB3::SpaceEditor printSpace $pdfobj $s curpage curline \
                      "$heading: Space [file rootname [file tail $s]]" \
                      -mapbundlemountpoint $mapbundlemountpoint \
                      -leveldir $leveldir -parent $temppath \
                      -mapeditor $mapeditor
                
                #	puts stderr "*** $type printLevel (after printSpace): curpage = $curpage, curline = $curline"
            }
            destroy $temppath
        }
        method print {} {
            ## Print this level (to a PDF file).
            
            #$options(-mapeditor) print
            set printfile "[file tail $currentLevelDir].pdf"
            if {"$printfile" eq ".pdf"} {set printfile "Level.pdf"}
            set pdfobj [$type drawPrintDialog \
                        -parent $win \
                        -what Level -filename $printfile]
            if {"$pdfobj" eq ""} {return}
            set heading $currentLevelDir
            set map [$levelframe getElementWidgetById map]
            set scalingfactor [expr {1.0 / double($oldscalefactor)}]
            $map scale all 0 0 $scalingfactor $scalingfactor
            $map configure -scrollregion [$map bbox all]
            $levelframe outputXMLToPDF $pdfobj "$heading: level info"
            set scalingfactor $zoomfactor
            $map scale all 0 0 $scalingfactor $scalingfactor
            $map configure -scrollregion [$map bbox all]
            set curpage [$levelframe getpageno]
            set curline [$levelframe getlineno]
            if {[$printSpacesLCB cget -text]} {
                foreach s [lsort [glob -nocomplain [file join $options(-leveldir) *.xml]]] {
                    if {[file tail $s] eq "levelinfo.xml"} {continue}
                    ::RolePlayingDB3::SpaceEditor printSpace $pdfobj $s curpage curline \
                          "$heading: Space [file rootname [file tail $s]]" \
                          -mapbundlemountpoint $options(-mapbundlemountpoint) \
                          -leveldir $options(-leveldir) -parent $win \
                          -mapeditor $options(-mapeditor)
                }
            }
            ::RolePlayingDB3::PrintDialog printprogress end      
            $pdfobj destroy
        }
        method close {args} {
            ## Close this level editor.
            # @param ... Options:
            # @arg -dontask Flag to supress asking.
            # @arg -dontsave Flag to supress saving.
            # @arg -closingallwindows Flag if we are closing all windows.
            
            set dontask [from args -dontask no]
            set dontsave [from args -dontsave no]
            if {[from args -closingallwindows no]} {
                $options(-mapeditor) close -closingallwindows yes
            }
            if {!$dontsave} {
                foreach se $spaceEditors {
                    if {[$se getdirty]} {
                        set isdirtyspace yes
                        break
                    }
                }
                if {!$dontask} {
                    if {$isdirty || $isdirtyspace} {
                        set ans [tk_messageBox -type yesnocancel -icon question \
                                 -parent $win \
                                 -message "Save data before closing window?"]
                        switch $ans {
                            yes {
                                if {[$self checksave] eq "error"} {return}
                                set dontask yes
                            }
                            cancel {return}
                            no {set dontsave yes}
                        }
                    }
                } else {
                    if {[$self checksave] eq "error"} {return}
                }
            }
            foreach se $spaceEditors {
                catch {$se close -dontsave $dontsave -dontask $dontask}
            }
            $options(-mapeditor) removeeditor [winfo toplevel $win]
            destroy [winfo toplevel $win]
        }
        constructor {args} {
            ## Constructor -- construct a level editor.
            # @param widgetpath Widget path.
            # @param ... Options:
            # @arg -template Template to use for a new level.
            # @arg -mapbundlemountpoint Map bundle mount point.
            # @arg -mapeditor Parent map editor.
            # @arg -leveldir Level directory.
            
            $self configurelist $args
            
            set currentLevelDir [file tail $options(-leveldir)]
            set mymediadir [file join $options(-mapbundlemountpoint) media \
                            [file tail $options(-leveldir)]]
            set mymediadirRelative [eval [list file join] [lrange [file split $mymediadir] 2 end]]
            [winfo toplevel $win] configure -title "Level Edit: $currentLevelDir"
            if {"$options(-template)" ne "" && ![file exists $options(-leveldir)]} {
                file mkdir $options(-leveldir)
                close [open [file join $options(-leveldir) flag] w]
                file mkdir $mymediadir
                close [open [file join $mymediadir flag] w]
                set XML $options(-template)
                set xmlfile [file join $options(-leveldir) levelinfo.xml]
                set firstsave yes
                set isnew yes
                set isdirty yes
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
            install banner using ttk::label $win.banner \
                  -image $bannerImage -anchor w \
                  -background $bannerBackground
            pack $banner -fill x  
            install toolbar using ButtonBox $win.toolbar -orient horizontal
            pack $toolbar -fill x
            $toolbar add ttk::button newspace -text {New Space} \
                  -command [mymethod _newspace]
            $toolbar add ttk::button delspace -text {Delete Space} \
                  -command [mymethod _deletespace]
            $toolbar add ttk::button editspace -text {Edit Space} \
                  -command [mymethod _editspace]
            install panes using ttk::panedwindow $win.panes -orient horizontal
            pack $panes -fill both -expand yes
            install spacetree using ::RolePlayingDB3::LabeledDirTree \
                  $panes.spacetree -showextension no -filepattern *.xml \
                  -label "Spaces"
            $panes add $spacetree -weight 1
            install levelframe using ::RolePlayingDB3::XMLContentEditor \
                  $panes.levelframe -xml $XML -isnewobject $isnew \
                  -dirtyvariable [myvar isdirty] \
                  -filewidgethandler [mymethod _filewidgethandler] \
                  -xmlfile $xmlfile \
                  -buttoncommand [mymethod _button] \
                  -basedirectory $options(-mapbundlemountpoint) \
                  -rootcontainer Level
            $panes add $levelframe -weight 5
            $spacetree configure -directory $options(-leveldir)
            $self updatelevelmap
            set zoomtobutton [$levelframe getElementWidgetById zoomto]
            #      puts stderr "*** $type create $self: zoomtobutton = $zoomtobutton"
            set zoommenu [menu $zoomtobutton.zoommenu -tearoff no]
            foreach zf {.0625 .125 .25 .5  1   2   4   8   16} \
                  lab {1:16 1:8  1:4 1:2 1 2:1 4:1 8:1 16:1} {
                $zoommenu add radiobutton -command [mymethod rescalelevelmap] \
                      -label "Zoom Factor $lab" \
                      -value $zf \
                      -variable [myvar zoomfactor]
            }
        }
        method updatespacetree {} {
            ## Update space tree.
            
            $spacetree redrawdirtree
        }
        method drawspace {canvas X Y color size args} {
            ## Draw the space.
            # @param canvas The canvas to draw to.
            # @param X The X coordinate.
            # @param Y The Y coordinate.
            # @param color The color.
            # @param size The size.
            # @param ... Options passed along to the map editor draw space 
            # method.
            
            $options(-mapeditor) drawspace $canvas $X $Y $color $size {*}$args
        }
        method updatelevelmapcolor {color spacetag} {
            ## Update the level map color.
            # @param color New color
            # @param spacetag The space tag.
            
            [$levelframe getElementWidgetById map] itemconfigure $spacetag \
                  -fill $color
        }
        method rescalelevelmap {} {
            ## Rescale the level map.
            
            set map [$levelframe getElementWidgetById map]
            set scalingfactor [expr {$zoomfactor / double($oldscalefactor)}]
            $map scale all 0 0 $scalingfactor $scalingfactor
            set oldscalefactor $zoomfactor
            $map configure -scrollregion [$map bbox all]
        }
        proc drawonespace_forprint {canvas spacefile mapeditor} {
            ## @privatesection Draw one space for printing.
            # @param canvas The canvas to draw onto.
            # @param spacefile The space file.
            # @param mapeditor The map editor.
            
            if {[catch {open $spacefile r} xmlfp]} {
                error "Illformed map bundle: $spacefile cannot be opened: $xmlfp"
            }
            set spacexml [read $xmlfp]
            close $xmlfp
            set X [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml "X Coord" 0]
            set Y [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml "Y Coord" 0]
            set color [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml "Color" white]
            foreach {xc yc} [$mapeditor scaleXY $X $Y 32] {break}
            $mapeditor drawspace $canvas $xc $yc $color 32 \
                  -tag "[file tail [file rootname $spacefile]]"
            
        }
        
        method drawonespace {X Y color spacetag} {
            ## @publicsection Draw one space
            # @param X The X coordinate.
            # @param Y The Y coordinate.
            # @param color The color.
            # @param spacetag The space tag.
            
            set map [$levelframe getElementWidgetById map]
            foreach {xc yc} [$options(-mapeditor) scaleXY $X $Y [expr {32 * $zoomfactor}]] {break}
            $self drawspace $map $xc $yc $color [expr {32 * $zoomfactor}] \
                  -tag "$spacetag"
            $map configure -scrollregion [$map bbox all]
        }
        method updatelevelmap {} {
            ## Update the level map.
            
            set map [$levelframe getElementWidgetById map]
            $map delete all
            foreach n [$spacetree children {}] {
                set fullpath [$spacetree itemcget $n -fullpath]
                if {[file tail $fullpath] eq "levelinfo.xml"} {
                    continue
                }
                if {[catch {open $fullpath r} xmlfp]} {
                    error "Illformed map bundle: $fullpath cannot be opened: $xmlfp"
                }
                set spacexml [read $xmlfp]
                close $xmlfp
                set X [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml "X Coord" 0]
                set Y [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml "Y Coord" 0]
                set color [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml "Color" white]
                foreach {xc yc} [$options(-mapeditor) scaleXY $X $Y [expr {32 * $zoomfactor}]] {break}
                $self drawspace $map $xc $yc $color [expr {32 * $zoomfactor}] \
                      -tag "[file tail [file rootname $fullpath]]"
            }
            $map configure -scrollregion [$map bbox all]
        }
        method _button {id} {
            ## @privatesection Button callback.
            # @param id The button id.
            
            switch $id {
                zoomin {
                    switch $zoomfactor {
                        .0625 {set zoomfactor .125}
                        .125  {set zoomfactor .25}
                        .25   {set zoomfactor .5}
                        .5    {set zoomfactor 1}
                        1	  {set zoomfactor 2}
                        2	  {set zoomfactor 4}
                        4	  {set zoomfactor 8}
                        8	  {set zoomfactor 16}
                    }
                    $self rescalelevelmap
                }
                zoomto {
                    set RX [winfo pointerx $levelframe]
                    set RY [winfo pointery $levelframe]
                    $zoommenu post $RX $RY
                }
                zoomout {
                    switch $zoomfactor {
                        .125  {set zoomfactor .0625}
                        .25   {set zoomfactor .125}
                        .5    {set zoomfactor .25}
                        1	  {set zoomfactor .5}
                        2	  {set zoomfactor 1}
                        4	  {set zoomfactor 2}
                        8	  {set zoomfactor 4}
                        16	  {set zoomfactor 8}
                    }
                    $self rescalelevelmap
                }
            }
        }
        method recreateXML {} {
            ## @publicsection Regenerate the XML string.
            
            set needmediatreeupdated no
            $levelframe recreateXML [file join $options(-leveldir) levelinfo.xml]
            if {$needmediatreeupdated} {$options(-mapeditor) updatemediatree}
        }
        method updatemediatree {} {
            ## Update the media tree.
            
            $options(-mapeditor) updatemediatree
        }
        method _filewidgethandler {curfile} {
            ## @privatesection File widget handler.
            # @param curfile The current file to handle.
            
            if {"$curfile" eq ""} {return $curfile}
            if {[file pathtype "$curfile"] eq "relative" &&
                "media" eq [lindex [file split $curfile] 0]} {
                return "$curfile"
            } else {
                file copy -force "$curfile" [file join $mymediadir [file tail $curfile]]
                set needmediatreeupdated yes
                return "[file join $mymediadirRelative [file tail $curfile]]"
            }
        }
        method removeeditor {se} {
            ## @publicsection Remove the selected editor.
            # @param se The space editor to remove.
            
            set seindex [lsearch -exact $spaceEditors $se]
            if {$seindex >= 0} {
                set spaceEditors [lreplace $spaceEditors $seindex $seindex]
            }
        }
        method _newspace {args} {
            ## @privatesection Create a new space.
            # @param ... Options:
            # @arg -parent The parent window.
            
            set parent [from args -parent $win]
            set space [$type draw_getSpaceFileDialog \
                       -parent $parent -title "Space to create" \
                       -bannerimage $newSpaceFileDialogBanner \
                       -root $options(-leveldir) -saveoropen save \
                       -filetypes {{{Space Files} *.xml TEXT}} \
                       -initialfile "New Space"]
            if {"$space" eq ""} {return}
            if {[file system [file dirname $space]] ne [file system $options(-leveldir)]} {
                tk_messageBox -parent $parent -type ok -icon info -message "Opps, you stepped off the internal file system!"
                return
            }
            if {![string match [file normalize [file join $options(-leveldir)]]/* [file normalize $space]]} {
                tk_messageBox -parent $parent -type ok -icon info -message "Opps, you stepped out of this level's folder!"
                return
            }
            if {[file normalize "$space"] eq [file normalize [file join $options(-leveldir) levelinfo.xml]]} {
                tk_messageBox -parent $parent -type ok -icon info -message "You cannot delete the levelinfo file!"
                return
            }
            if {[file exists "$space"]} {
                tk_messageBox -parent $parent -type ok -icon info -message "Space [file rootname [file tail $space]] already exists!"
                return
            }
            set newspaceeditor [::RolePlayingDB3::SpaceEditor new \
				-mapbundlemountpoint $options(-mapbundlemountpoint) \
				-leveldir $options(-leveldir) \
				-parent $parent -newspace $space \
				-leveleditor [winfo toplevel $win]]
            if {"$newspaceeditor" ne ""} {
                set isdirty yes
                $self updatespacetree
                lappend spaceEditors $newspaceeditor
            }
        }
        method _deletespace {} {
            ## Delete a selected space.
            
            if {[llength [glob -nocomplain -type f [file join $options(-leveldir) *.xml]]] == 1} {
                return
            }
            set selection [$spacetree selection]
            if {[llength $selection] > 0} {
                set space [$spacetree itemcget [lindex $selection 0] -fullpath]
            } else {
                set space [$type draw_getSpaceFileDialog \
                           -parent $parent -title "Space to create" \
                           -bannerimage $oldSpaceFileDialogBanner \
                           -root $options(-leveldir) -saveoropen open \
                           -filetypes {{{Space Files} *.xml TEXT}}]
            }
            if {"$space" eq ""} {return}
            if {[file system [file dirname $space]] ne [file system $options(-leveldir)]} {
                tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
                return
            }
            if {![string match [file normalize [file join $options(-leveldir)]]/* [file normalize $space]]} {
                tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of this level's folder!"
                return
            }
            if {[file normalize "$space"] eq [file normalize [file join $options(-leveldir) levelinfo.xml]]} {
                tk_messageBox -parent $win -type ok -icon info -message "You cannot delete the levelinfo file!"
                return
            }
            set selist $spaceEditors
            foreach se $spaceEditors {
                if {[file normalize "$space"] eq [file normalize "[$se getspacefile]"]} {
                    $se close -dontsave yes
                    set seindex [lsearch -exact $selist $se]
                    if {$seindex >= 0} {set selist [lreplace $seindex $seindex $seindex]}
                }
            }
            set spaceEditors $selist
            file delete $space
            $self updatespacetree
            $self updatemediatree
            $self updateleveltree
            $self updatelevelmap
            set isdirty yes      
        }
        method _editspace {args} {
            ## Edit a selected space.
            # @param ... Options:
            # @arg -parent The parent window.
            
            set parent [from args -parent $win]
            if {[llength [glob -nocomplain -type f [file join $options(-leveldir) *.xml]]] == 1} {
                return
            }
            set selection [$spacetree selection]
            if {[llength $selection] > 0} {
                set space [$spacetree itemcget [lindex $selection 0] -fullpath]
            } else {
                set space [$type draw_getSpaceFileDialog \
                           -parent $parent -title "Space to create" \
                           -bannerimage $oldSpaceFileDialogBanner \
                           -root $options(-leveldir) -saveoropen open \
                           -filetypes {{{Space Files} *.xml TEXT}}]
            }
            if {"$space" eq ""} {return}
            if {[file system [file dirname $space]] ne [file system $options(-leveldir)]} {
                tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
                return
            }
            if {![string match [file normalize [file join $options(-leveldir)]]/* [file normalize $space]]} {
                tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of this level's folder!"
                return
            }
            if {[file normalize "$space"] eq [file normalize [file join $options(-leveldir) levelinfo.xml]]} {
                tk_messageBox -parent $win -type ok -icon info -message "You cannot use the space editor on the levelinfo file!"
                return
            }
            foreach se $spaceEditors {
                if {[file normalize "$space"] eq [file normalize "[$se getspacefile]"]} {
                    wm deinconify $se
                    raise $se
                    return
                }
            }
            set newspaceeditor [::RolePlayingDB3::SpaceEditor open \
				-mapbundlemountpoint $options(-mapbundlemountpoint) \
				-leveldir $options(-leveldir) \
				-parent $parent -spacefile $space \
				-leveleditor [winfo toplevel $win]]
            if {"$newspaceeditor" ne ""} {
                lappend spaceEditors $newspaceeditor
            }
        }
        method updateleveltree {} {
            ## @publicsection Update the level tree.
            
            $options(-mapeditor) updateleveltree
        }
        method updatemediatree {} {
            ## Update the media tree.
            
            $options(-mapeditor) updatemediatree
        }
        variable _positionSelected
        ## @privatesection The selected position.
        method setspaceposition {XLE YLE spacename} {
            ## @publicsection Set the space's position.
            # @param XLE The X position entry.
            # @param YLE The Y position entry.
            # @param spacename The space name.
            
            set map [$levelframe getElementWidgetById map]
            set topY    [$map canvasy 0]
            set bottomY [$map canvasy [winfo height $map]]
            set leftX   [$map canvasx 0]
            set rightX  [$map canvasx [winfo width $map]]
            $map create line $leftX $topY $leftX $bottomY -fill black -tag vert
            $map create line $leftX $topY $rightX $topY -fill black -tag horiz
            $map bindcanvas <Motion> [mymethod _drawXHairs %W %x %y]
            $map bindcanvas <1>      [mymethod _selectPosition %W %x %y $XLE $YLE $spacename]
            $map bindcanvas <3>      [mymethod _cancelSelectPosition %W]
            $map bindcanvas <Escape> [mymethod _cancelSelectPosition %W]
            raise [winfo toplevel $win]
            set oldgrab [grab current [winfo toplevel $win]]
            grab set [winfo toplevel $win]
            set _positionSelected 0
            #puts stderr "*** $self setspaceposition (before vwait): _positionSelected = $_positionSelected"
            vwait [myvar _positionSelected]
            #puts stderr "*** $self setspaceposition (after vwait): _positionSelected = $_positionSelected"
            grab release [winfo toplevel $win]
            if {$oldgrab ne ""} {grab set $oldgrab}
            $map bindcanvas <Motion> {}
            $map bindcanvas <1>      {}
            $map bindcanvas <3>      {}
            $map bindcanvas <Escape> {}
            raise [winfo toplevel $XLE]
            focus $XLE
        }
        method _drawXHairs {canvas mx my} {
            ## @privatesection Draw the cross hairs.
            # @param canvas The canvas to draw on.
            # @param mx The mouse X coordinate.
            # @param my The mouse Y coordinate.
            
            #      puts stderr "*** $self _drawXHairs $canvas $mx $my"
            set curX [$canvas canvasx $mx]
            set curY [$canvas canvasy $my]
            set topY    [$canvas canvasy 0]
            set bottomY [$canvas canvasy [winfo height $canvas]]
            set leftX   [$canvas canvasx 0]
            set rightX  [$canvas canvasx [winfo width $canvas]]
            $canvas coords vert $curX $topY $curX $bottomY
            $canvas coords horiz $leftX $curY $rightX $curY
        }
        method checkoccupiedspace {X Y spacefile} {
            ## Check for an occupied space.
            # @param X The X coordinate.
            # @param Y The Y coordinate.
            # @param spacefile The spacefile.
            # @returns a boolean value indicating if the space is already 
            # occupied.
            
            foreach s [glob -nocomplain [file join $options(-leveldir) *.xml]] {
                if {[file tail $s] eq "levelinfo.xml"} {continue}
                if {[file tail $s] eq [file tail $spacefile]} {continue}
                if {[catch {open $spacefile r} xmlfp]} {
                    error "Illformed map bundle: $spacefile cannot be opened: $xmlfp"
                }
                set spacexml [read $xmlfp]
                close $xmlfp
                set Xother [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml "X Coord" 0]
                set Yother [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml "Y Coord" 0]
                if {$X == $Xother && $Y == $Yother} {
                    return yes
                }
            }
            return no
        }
        method _selectPosition {canvas mx my xLE yLE spacefile} {
            ## @privatesection Select a position.
            # @param canvas The canvas to draw on.
            # @param mx The mouse X coordinate.
            # @param my The mouse Y coordinate.
            # @param xLE The X coordinate entry.
            # @param yLE The Y coordinate entry.
            # @param spacefile The space file.
            
            #      puts stderr "*** $self _selectPosition $canvas $mx $my $xLE $yLE"
            set curX [$canvas canvasx $mx]
            set curY [$canvas canvasy $my]
            foreach {X Y} [$options(-mapeditor) unscaleXY $curX $curY [expr {$zoomfactor * 32}]] {break}
            if {[$self checkoccupiedspace $X $Y $spacefile]} {
                tk_messageBox -type ok -icon warning -message "Cannot have two spaces at the same location!"
                return
            }
            $xLE configure -text $X
            $yLE configure -text $Y
            $canvas delete vert
            $canvas delete horiz
            incr _positionSelected
        }
        method _cancelSelectPosition {canvas} {
            ## Cancel select position.
            # @param canvas The canvas.
            
            #      puts stderr "*** $self _cancelSelectPosition $canvas"
            $canvas delete vert
            $canvas delete horiz
            incr _positionSelected -1
        }
    }
    snit::widget SpaceEditor {
        ## Space edit widget.
        option -template -readonly yes -default {}
        ::RolePlayingDB3::MapBundleMountPoint
        ::RolePlayingDB3::LevelEditorOption
        ::RolePlayingDB3::SpaceFileOption
        variable currentSpaceFile
        ## @privatesection The current space file.
        method getspacefile {} {
            ## @publicsection Retrieve the current space file.
            # @returns the current space file.

            return $currentSpaceFile
        }
        variable firstsave yes
        ## @privatesection First save flag.
        variable isdirty no
        ## Dirty flag.
        variable needmediatreeupdated no
        ## Update media flag.
        variable zoommenu
        ## Zoom menu.
        variable zoomfactor 1
        ## Zoom factior.
        variable mymediadir
        ## My media dir.
        variable mymediadirRelative
        ## My media dir, relative.
        method setdirty {} {
            ## @publicsection Set the dirty flag.
            
            set isdirty yes
        }
        method checksave {} {
            ## Check if saving is needed.
            
            if {$isdirty} {
                if {$firstsave} {
                    set XLE [$spaceframe getElementWidgetById X]
                    set YLE [$spaceframe getElementWidgetById Y]
                    set X [$XLE cget -text]
                    set Y [$YLE cget -text]
                    if {[$options(-leveleditor) checkoccupiedspace $X $Y $options(-spacefile)]} {
                        tk_messageBox -type ok -icon warning -message "Cannot have two spaces at the same location!"
                        return error
                    }
                }
                $self recreateXML
                if {$firstsave} {
                    $options(-leveleditor) updatelevelmap
                    set firstsave no
                }
                set isdirty no
            }
            $options(-leveleditor) setdirtyspace
            return ok
        }
        variable spacecanvas
        ## @privatesection The space canvas.
        
        component banner
        ## Banner.
        component toolbar
        ## Toolbar.
        component spaceframe
        ## Space frame
        
        component _itemDialog
        ## Item dialog.
        component   i_nameLE
        ## Item name LE.
        component   i_descrLE
        ## Item description LE.
        component   i_xLE
        ## Item X LE.
        component   i_yLE
        ## Item Y LE.
        component   i_imfileFE
        ## Item image file FE.
        component   i_sheetFileFE
        ## Item sheet file FE.
        
        component _exitDialog
        ## Exit dialog.
        component   e_nameLE
        ## Exit name LE.
        component   e_descrLE
        ## Exit description LE.
        component   e_xLE
        ## Exit X LE.
        component   e_yLE
        ## Exit Y LE.
        component   e_imfileFE
        ## Exit image file FE.
        component   e_otherSpaceNameLF
        ## Exit to other space name LF.
        component     e_otherSpaceNameE
        ## Exit to other space name Entry.
        component     e_otherSpaceNameB
        ## Exit to other space name Button.
        component     e_otherSpaceName_nameDialog
        ## Exit to other space name dialog.
        component   e_sheetFileFE
        ## Exit sheet file FE.
        typevariable otherSpaceNameDialogBanner
        ## Other space name dialog banner.

        proc isodd {n} {
            ## Check if a number is odd.
            # @param n The number to check.
            # @returns a boolean value, true if n is odd.
            
            return [expr {($n % 1) == 1}]
        }
        proc getFromAttrList {key attrlist {default {}}} {
            ## Get a key's value from an attrlist.
            # @param key The key to lookup.
            # @param attrlist The attribute list.
            # @param default Value to return if key is not in the attrlist.
            # @returns the key's value or the default value.
            
            #      puts stderr "*** [list getFromAttrList $key $attrlist $default]"
            set index [lsearch $attrlist $key]
            #      puts stderr "*** getFromAttrList: index = $index"
            if {$index < 0 || [isodd $index]} {
                return $default
            } else {
                incr index
                return [lindex $attrlist $index]
            }
        }
        proc insertOrReplaceInAttrList {key value attrlistVar} {
            ## Insert or replace a key's value in an attrlist.
            # @param key The key to insert or replace.
            # @param value The new value.
            # @param attrlistVar The name of the variable holding the attrlist.
            
            #      puts stderr "*** [list insertOrReplaceInAttrList $key $value $attrlistVar]"
            upvar $attrlistVar attrlist
            set index [lsearch $attrlist $key]
            #      puts stderr "*** insertOrReplaceInAttrList: index = $index"
            if {$index < 0 || [isodd $index]} {
                lappend attrlist $key "$value"
            } else {
                incr index
                set attrlist [lreplace $attrlist $index $index "$value"]
            }
            #      puts stderr "*** insertOrReplaceInAttrList: attrlist = $attrlist"
        }
        
        typevariable bannerImage
        ## The banner image.
        typevariable bannerBackground #a2de86
        ## The banner background color.
        typevariable spaceTemplateXML {<?xml version="1.0" ?>
            <rpgv3:Space xmlns:rpgv3="http://www.deepsoft.com/roleplayingdb/v3xmlns"
            name="Space" >Space Information
            <rpgv3:Field name="Title" type="Word / Short Phrase" updatable="yes" />
            <rpgv3:Field name="X Coord" type="Whole Number" updatable="no" range="-1000 1000 1" id="X" />
            <rpgv3:Field name="Y Coord" type="Whole Number" updatable="no" range="-1000 1000 1" id="Y" />
            <rpgv3:Field name="Color" type="Color" updatable="yes" id="color" />
            <rpgv3:Field name="Description" type="Long Text" updatable="yes" />
            <rpgv3:SpaceContents name="Space Contents">Space Contents
            <rpgv3:Items name="Items" side="right">List of items
            <rpgv3:List id="itemlist" />
            <rpgv3:Buttonbox orient="horizontal" fill="x" expand="no" >
            <rpgv3:Bi label="Add New Item" id="addnewitem" />
            <rpgv3:Bi label="Edit Item"    id="edititem" />
            <rpgv3:Bi label="Delete Item"  id="deleteitem" />
            </rpgv3:Buttonbox>
            </rpgv3:Items>
            <rpgv3:Exits name="Exits" side="left" >List of exits
            <rpgv3:List id="exitlist" />
            <rpgv3:Buttonbox orient="horizontal" fill="x" expand="no" >
            <rpgv3:Bi label="Add New Exit" id="addnewexit" />
            <rpgv3:Bi label="Edit Exit"    id="editexit" />
            <rpgv3:Bi label="Delete Exit"  id="deleteexit" />
            </rpgv3:Buttonbox>
            </rpgv3:Exits>
            </rpgv3:SpaceContents >
            <rpgv3:SpaceMap name="Space Map">Space Map
            <rpgv3:Canvas name="Space Map Canvas" id="map" background="gray" side="left" />
            <rpgv3:Buttonbox orient="vertical" fill="both" side="right" expand="no" id="zoombuttons" >
            <rpgv3:Bi label="Zoom In"  id="zoomin" />
            <rpgv3:Bi label="Zoom &gt;" id="zoomto" />
            <rpgv3:Bi label="Zoom Out" id="zoomout" />
            </rpgv3:Buttonbox>
            </rpgv3:SpaceMap>
            </rpgv3:Space>
        }
        ## The template for a new space.
        typeconstructor {
            ## Type constructor: one time initialization.
            
            set bannerImage [image create photo \
                             -file [file join $::RolePlayingDB3::ImageDir \
                                    SpaceBanner.png]]
            set otherSpaceNameDialogBanner [image create photo \
                                            -file [file join $::RolePlayingDB3::ImageDir \
                                                   OtherSpaceNameBanner.png]]
        }
        method new {} {
            ## @publicsection Create a new space.
            
            $options(-leveleditor) _newspace -parent $win
        }
        typemethod new {args} {
            ## Create a new space.
            # @param ... Options:
            # @arg -mapbundlemountpoint The map bundle mount point.
            # @arg -parent The parent window.
            # @arg -leveleditor The level editor.
            # @arg -leveldir The level directory.
            # @arg -newspace The new space file.
            # @par
            # @returns the new toplevel.
            
            set mapbundlemountpoint [from args -mapbundlemountpoint]
            set parent [from args -parent .]
            set leveleditor [from args -leveleditor]
            set leveldir [from args -leveldir]
            set space [from args -newspace]
            set newTop [RolePlayingDB3::RPGToplevel \
                        .space%AUTO% \
                        -mainframeconstructor $type \
                        -mainframetemplate $spaceTemplateXML \
                        -class SpaceEditor \
                        -mapbundlemountpoint $mapbundlemountpoint \
                        -leveleditor $leveleditor -spacefile $space -minwidth 800]
            return $newTop
        }
        method open {} {
            ## Open an existing space file.
            
            $options(-leveleditor) _editspace -parent $win
        }
        typemethod open {args} {
            ## Open an existing space file.
            # @param ... Options:
            # @arg -mapbundlemountpoint The map bundle mount point.
            # @arg -parent The parent window.
            # @arg -leveleditor The level editor.
            # @arg -leveldir The level directory.
            # @arg -spacefile The new space file.
            # @par
            # @returns the new toplevel.
            
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
                        -leveleditor $leveleditor -minwidth 800]
            return $newTop
        }
        method save {} {
            ## Save the current space to the current file.
            
            $options(-leveleditor) save
        }
        method saveas {{_filename {}}} {
            ## Save the current space to a new file.
            # @param _filename The file name to save to.
            
            $options(-leveleditor) saveas $_filename
        }
        typemethod printSpace {pdfobj spacefile curpageV curlineV heading args} {
            ## Print a space (to a PDF file).
            # @param pdfobj The PDF Object to print to.
            # @param spacefile The space file to print.
            # @param curpageV The current page variable.
            # @param curlineV The current line variable.
            # @param heading The heading.
            # @param ... Options:
            # @arg -mapbundlemountpoint The map bundle mount point.
            # @arg -leveldir The level directory.
            # @arg -parent The parent window.
            # @arg -mapeditor The parent map editor.
            
            upvar $curpageV curpage
            upvar $curlineV curline
            #      puts stderr "*** $type printSpace (on entrance): curpage = $curpage, curline = $curline"
            set mapbundlemountpoint [from args -mapbundlemountpoint]
            set leveldir	      [from args -leveldir]
            set parent              [from args -parent]
            set mapeditor	      [from args -mapeditor]
            if {[catch {open $spacefile r} shfp]} {
                error "Illformed map bundle: $spacefile cannot be opened: $shfp"
            }
            set XML [read $shfp]
            close $shfp
            set temppath [::RolePlayingDB3::XMLContentEditor $parent.tempspace%AUTO% \
                          -xml $XML -isnewobject no -dirtyvariable {} \
                          -filewidgethandler {}  -xmlfile {} \
                          -rootcontainer Space -basedirectory $mapbundlemountpoint]
            drawspace [$temppath getElementWidgetById map] \
                  [$temppath getElementWidgetById itemlist] \
                  [$temppath getElementWidgetById exitlist] \
                  $mapeditor \
                  [[$temppath getElementWidgetById color] cget -text] \
                  1.0 $mapbundlemountpoint
            $temppath outputXMLToPDF $pdfobj $heading $curpage $curline
            set curpage [$temppath getpageno]
            set curline [$temppath getlineno]
            #      puts stderr "*** $type printSpace (on exit): curpage = $curpage, curline = $curline"
            destroy $temppath
        }
        method print {} {
            ## Print the current space.
            
            set printfile "[file rootname [file tail $currentSpaceFile]].pdf"
            if {"$printfile" eq ".pdf"} {set printfile "Space.pdf"}
            set pdfobj [::RolePlayingDB3::PrintDialog drawPrintDialog \
                        -what Space -filename $printfile]
            
            if {"$pdfobj" eq ""} {return}
            set heading $currentSpaceFile
            set savedzoomfactor $zoomfactor
            set zoomfactor 1.0
            $self redrawspace
            $spaceframe outputXMLToPDF $pdfobj "$heading: space info"
            set zoomfactor $savedzoomfactor
            $self redrawspace
            ::RolePlayingDB3::PrintDialog printprogress end      
            $pdfobj destroy
        }
        method close {args} {
            ## Close the current space.
            # @param ... Options:
            # @arg -dontask Flag to supress asking.
            # @arg -dontsave Flag to supress saving.
            # @arg -closingallwindows Flag to indicating that all windows are 
            # being closed.

            set dontask [from args -dontask no]
            set dontsave [from args -dontsave no]
            if {[from args -closingallwindows no]} {
                $options(-leveleditor) close -closingallwindows yes
            }
            if {!$dontsave} {
                if {!$dontask} {
                    if {$isdirty} {
                        set ans [tk_messageBox -type yesnocancel -icon question \
                                 -parent $win \
                                 -message "Save data before closing window?"]
                        switch $ans {
                            yes {$self checksave}
                            cancel {return}
                            no {set dontsave yes}
                        }
                    }
                } else {
                    $self checksave
                }
            }
            $options(-leveleditor) removeeditor [winfo toplevel $win]
            destroy [winfo toplevel $win]
        }
        constructor {args} {
            ## Constructor -- construct a space edit widget.
            # @param widgetpage The widget path.
            # @param ... Options:
            # @arg -template The template XML to use for a new space.
            # @arg -mapbundlemountpoin The map bundle mount point.
            # @arg -leveleditor The level editor.
            # @arg -spacefile The space filename.
            
            $self configurelist $args
            
            set currentSpaceFile [file rootname [file tail $options(-spacefile)]]
            set mymediadir [file join $options(-mapbundlemountpoint) media \
                            [file tail [file dirname \
                                        $options(-spacefile)]] \
                            $currentSpaceFile]
            set mymediadirRelative [eval [list file join] [lrange [file split $mymediadir] 2 end]]
            [winfo toplevel $win] configure -title "Space Edit: $currentSpaceFile"
            if {"$options(-template)" ne "" && ![file exists $options(-spacefile)]} {
                file mkdir $mymediadir
                close [open [file join $mymediadir flag] w]
                set XML $options(-template)
                set firstsave yes
                set isnew yes
                set xmlfile $options(-spacefile)
                set isdirty yes
            } elseif {[file exists $options(-spacefile)]} {
                set xmlfile {}
                if {[catch {open $options(-spacefile) r} shfp]} {
                    error "Illformed map bundle: $options(-spacefile) cannot be opened: $shfp"
                }
                set XML [read $shfp]
                close $shfp
                set firstsave no
                set isnew no
            } else {
                error "Neither -template nor a valid -spacefile was passed!"
            }
            install banner using ttk::label $win.banner \
                  -image $bannerImage -anchor w \
                  -background $bannerBackground  
            pack $banner -fill x
            install toolbar using ButtonBox $win.toolbar -orient horizontal
            pack $toolbar -fill x
            $toolbar add ttk::button positionspace -text {Set Space Position} \
                  -command [mymethod _setspaceposition] \
                  -state disabled
            if {$isnew} {$toolbar itemconfigure 0 -state normal}
            install spaceframe using ::RolePlayingDB3::XMLContentEditor \
                  $win.spaceframe -xml $XML -isnewobject $isnew \
                  -dirtyvariable [myvar isdirty] \
                  -filewidgethandler [mymethod _filewidgethandler] \
                  -xmlfile $xmlfile \
                  -buttoncommand [mymethod _button] \
                  -basedirectory $options(-mapbundlemountpoint) \
                  -rootcontainer Space
            pack $spaceframe -fill both -expand yes
            set spacecanvas [$spaceframe getElementWidgetById map]
            set zoomtobutton [$spaceframe getElementWidgetById zoomto]
            #      puts stderr "*** $type create $self: zoomtobutton = $zoomtobutton"
            set zoommenu [menu $zoomtobutton.zoommenu -tearoff no]
            foreach zf {.0625 .125 .25 .5  1   2   4   8   16} \
                  lab {1:16 1:8  1:4 1:2 1 2:1 4:1 8:1 16:1} {
                $zoommenu add radiobutton -command [mymethod redrawspace] \
                      -label "Zoom Factor $lab" \
                      -value $zf \
                      -variable [myvar zoomfactor]
            }
            set cw [$spaceframe getElementWidgetById color]
            $cw configure -modifycmd [mymethod colorchanged]
            $cw bind <Return> [mymethod colorchanged]
            $self redrawspace
            $options(-leveleditor) updatelevelmap
            if {$isnew} {
                $options(-leveleditor) updateleveltree
                $options(-leveleditor) updatemediatree
            }
            $self createdialogs
            update
        }
        method colorchanged {} {
            ## Update on color change.
            
            if {![catch {$spacecanvas itemconfigure background -fill [$self getcolor]}]} {
                $options(-leveleditor) updatelevelmapcolor \
                      [$self getcolor] \
                      [file rootname [file tail $options(-spacefile)]]
                set isdirty yes
            }
        } 
        method getcolor {} {
            ## Get the current color.
            # @returns the current color.
            
            return [[$spaceframe getElementWidgetById color] cget -text]
        }
        method _button {id} {
            ## @privatesection Button handler.
            # @param id The button id to handle.
            
            switch $id {
                addnewitem {
                    $self _addnewitem
                }
                edititem {
                    $self _edititem
                }
                deleteitem {
                    $self _deleteitem
                }
                addnewexit {
                    $self _addnewexit
                }
                editexit {
                    $self _editexit
                }
                deleteexit {
                    $self _deleteexit
                }
                zoomin {
                    switch $zoomfactor {
                        .0625 {set zoomfactor .125}
                        .125  {set zoomfactor .25}
                        .25   {set zoomfactor .5}
                        .5    {set zoomfactor 1}
                        1	  {set zoomfactor 2}
                        2	  {set zoomfactor 4}
                        4	  {set zoomfactor 8}
                        8	  {set zoomfactor 16}
                    }
                    $self redrawspace
                }
                zoomto {
                    set RX [winfo pointerx $spaceframe]
                    set RY [winfo pointery $spaceframe]
                    $zoommenu post $RX $RY
                }
                zoomout {
                    switch $zoomfactor {
                        .125  {set zoomfactor .0625}
                        .25   {set zoomfactor .125}
                        .5    {set zoomfactor .25}
                        1	  {set zoomfactor .5}
                        2	  {set zoomfactor 1}
                        4	  {set zoomfactor 2}
                        8	  {set zoomfactor 4}
                        16	  {set zoomfactor 8}
                    }
                    $self redrawspace
                }
            }
        }
        proc drawspace {spacecanvas itemlist exitlist editor color zoomfactor mp} {
            ## Draw this space.
            # @param spacecanvas The canvas to draw on.
            # @param itemlist The items to draw.
            # @param exitlist The exits to draw.
            # @param editor The editor.
            # @param color Space background color.
            # @param zoomfactor The zoom factor.
            # @param mp The mount point.
            
            $spacecanvas delete all
            $editor drawspace $spacecanvas 0 0 $color \
                  [expr {$zoomfactor * 320}] \
                  -tag background
            foreach i [$itemlist items] {
                set iData [lindex [$itemlist itemcget $i -values] 1]
                set xc [expr {$zoomfactor * [getFromAttrList X $iData 0]}]
                set yc [expr {$zoomfactor * [getFromAttrList Y $iData 0]}]
                set imfile [file join $mp [getFromAttrList imfile $iData]]
                $spacecanvas create image $xc $yc -image [image create photo -file $imfile]
            }
            foreach e [$exitlist items] {
                set eData [lindex [$exitlist itemcget $e -values] 1]
                set xc [expr {$zoomfactor * [getFromAttrList X $eData 0]}]
                set yc [expr {$zoomfactor * [getFromAttrList Y $eData 0]}]
                set imfile [file join $mp [getFromAttrList imfile $eData]]
                $spacecanvas create image $xc $yc -image [image create photo -file $imfile]
            }
            $spacecanvas configure -scrollregion [$spacecanvas bbox all]
        }
        method redrawspace {} {
            ## @publicsection Redraw the space.
            
            drawspace $spacecanvas [$spaceframe getElementWidgetById itemlist] \
                  [$spaceframe getElementWidgetById exitlist] \
                  $options(-leveleditor) \
                  [$self getcolor] \
                  $zoomfactor \
                  $options(-mapbundlemountpoint)
        }
        method recreateXML {} {
            ## Regenerate the XML string.
            
            set needmediatreeupdated no
            $spaceframe recreateXML $options(-spacefile)
            if {$needmediatreeupdated} {$options(-leveleditor) updatemediatree}
        }
        method _filewidgethandler {curfile} {
            ## @privatesection File widget handler.
            # @param curfile The current file to be handled.
            
            if {"$curfile" eq ""} {return $curfile}
            if {[file pathtype "$curfile"] eq "relative" &&
                "media" eq [lindex [file split $curfile] 0]} {
                return "$curfile"
            } else {
                file copy -force "$curfile" [file join $mymediadir [file tail $curfile]]
                set needmediatreeupdated yes
                return "[file join $mymediadirRelative [file tail $curfile]]"
            }
        }
        method createdialogs {} {
            ## Create dialogs.
            
            set baseW $win
            install _itemDialog using Dialog $baseW.itemDialog \
                  -image [::RolePlayingDB3::MapEditor getDialogIcon] \
                  -cancel 1 -default 0 -modal local \
                  -parent [winfo toplevel $win] -side bottom \
                  -title "Add New Item" -transient yes
            $_itemDialog add new    -text {Create}
            $_itemDialog add cancel -text {Cancel}
            set frame [$_itemDialog getframe]
            install i_nameLE using LabelEntry $frame.nameLE -label "Name:" \
                  -labelwidth 10
            pack $i_nameLE -fill x
            install i_descrLE using LabelEntry $frame.descrLE -label "Description:" \
                  -labelwidth 10
            pack $i_descrLE -fill x
            install i_xLE using LabelSpinBox $frame.xLE -label "X:" -labelwidth 10 \
                  -range {-320 320 1}
            pack $i_xLE -fill x
            install i_yLE using LabelSpinBox $frame.yLE -label "Y:" -labelwidth 10 \
                  -range {-320 320 1}
            pack $i_yLE -fill x
            install i_imfileFE using FileEntry $frame.imfileFE -label "Image:" \
                  -labelwidth 10 \
                  -filetypes  { 
                {"BMP Files"	{.bmp}	      }
                {"GIF Files"	{.gif .GIF}     GIFf}
                {"JPEG Files"       {.jpeg .jpg}    JPEG}
                {"PNG Files"	{.png}	  PNGF}
                {"TIFF Files"       {.tiff .tif}    TIFF}
                {"XBM Files"	{.xbm}	      }
                {"XPM Files"	{.xpm}	      }
                {"Postscript Files" {.ps .eps}	  } 
                {"All Files"	*		   } }
            pack $i_imfileFE -fill x
            install i_sheetFileFE using FileEntry $frame.sheetFileFE -label "Sheet File:" \
                  -labelwidth 10 \
                  -filetypes  {
                {"Sheet Files"      {*.rpg}	     }
                {"All Files"	*		   } }
            pack $i_sheetFileFE -fill x
            install _exitDialog using Dialog $baseW.exitDialog \
                  -image [::RolePlayingDB3::MapEditor getDialogIcon] \
                  -cancel 1 -default 0 -modal local \
                  -parent [winfo toplevel $win] -side bottom \
                  -title "Add New Exit" -transient yes
            $_exitDialog add new    -text {Create}
            $_exitDialog add cancel -text {Cancel}
            set frame [$_exitDialog getframe]
            install e_nameLE using LabelEntry $frame.nameLE -label "Name:" \
                  -labelwidth 14
            pack $e_nameLE -fill x
            install e_descrLE using LabelEntry $frame.descrLE -label "Description:" \
                  -labelwidth 14
            pack $e_descrLE -fill x
            install e_xLE using LabelSpinBox $frame.xLE -label "X:" -labelwidth 14 \
                  -range {-320 320 1}
            pack $e_xLE -fill x
            install e_yLE using LabelSpinBox $frame.yLE -label "Y:" -labelwidth 14 \
                  -range {-320 320 1}
            pack $e_yLE -fill x
            install e_imfileFE using FileEntry $frame.imfileFE -label "Image:" \
                  -labelwidth 14 \
                  -filetypes  { 
                {"BMP Files"	{.bmp}	      }
                {"GIF Files"	{.gif .GIF}     GIFf}
                {"JPEG Files"       {.jpeg .jpg}    JPEG}
                {"PNG Files"	{.png}	  PNGF}
                {"TIFF Files"       {.tiff .tif}    TIFF}
                {"XBM Files"	{.xbm}	      }
                {"XPM Files"	{.xpm}	      }
                {"Postscript Files" {.ps .eps}	  } 
                {"All Files"	*		   } }
            pack $e_imfileFE -fill x
            install e_otherSpaceNameLF using LabelFrame $frame.otherSpaceNameLF \
                  -text "Exit to space:" -width 14
            pack $e_otherSpaceNameLF -fill x
            set e_otherSpaceNameLF_f [$e_otherSpaceNameLF getframe]
            install e_otherSpaceNameE using ttk::entry $e_otherSpaceNameLF_f.entry \
                  -text $options(-spacefile)
            pack $e_otherSpaceNameE -side left -fill x -expand yes
            install e_otherSpaceNameB using ttk::button $e_otherSpaceNameLF_f.browse \
                  -command [mymethod _browseOtherSpace] \
                  -image [IconImage image openfold]
            pack $e_otherSpaceNameB -side right
            install e_otherSpaceName_nameDialog \
                  using ::RolePlayingDB3::chroot_getFile \
                  $e_otherSpaceNameLF_f._nameDialog \
                  -bannerimage $otherSpaceNameDialogBanner \
                  -bannerbackground $bannerBackground \
                  -saveoropen open \
                  -filetypes { {{Space XML Files} {.xml} TEXT} } \
                  -defaultextension .xml \
                  -parent [winfo toplevel $win] \
                  -root [file dirname [file dirname $options(-spacefile)]] \
                  -initialfile $options(-spacefile) \
                  -initialdir [file dirname $options(-spacefile)] \
                  -title {Other space XML file}
            install e_sheetFileFE using FileEntry $frame.sheetFileFE -label "Sheet File:" \
                  -labelwidth 14 \
                  -filetypes  {
                {"Sheet Files"      {*.rpg}	     }
                {"All Files"	*		   } }
            pack $e_sheetFileFE -fill x
        }
        method _browseOtherSpace {} {
            ## Browse other space.
            
            set curfile [$e_otherSpaceNameE cget -text]
            set newfile [$e_otherSpaceName_nameDialog draw -initialfile "$curfile"]
            if {"$newfile" eq ""} {return}
            $e_otherSpaceNameE delete 0 end
            $e_otherSpaceNameE insert end "$newfile"
        }
        method _addnewitem {} {
            ## Add a new item.
            
            set itemlist [$spaceframe getElementWidgetById itemlist]
            $_itemDialog itemconfigure 0 -text {Create}
            set ans [$_itemDialog draw]
            if {$ans == 1} {return}
            set attrList [list]
            insertOrReplaceInAttrList name "[$i_nameLE cget -text]" attrList
            set descr "[$i_descrLE cget -text]"
            insertOrReplaceInAttrList X "[$i_xLE cget -text]" attrList
            insertOrReplaceInAttrList Y "[$i_yLE cget -text]" attrList
            set needmediatreeupdated no
            insertOrReplaceInAttrList imfile [$self _filewidgethandler "[$i_imfileFE cget -text]"] attrList
            insertOrReplaceInAttrList sheet [$self _filewidgethandler "[$i_sheetFileFE cget -text]"] attrList
            #      puts stderr "*** $self _addnewitem: attrList = $attrList"
            $itemlist insert end -text "$descr" -values [list $descr "$attrList"]
            $self redrawspace
            set isdirty yes
            if {$needmediatreeupdated} {$options(-leveleditor) updatemediatree}
        }
        method _edititem {} {
            ## Edit an existing item.
            
            set itemlist [$spaceframe getElementWidgetById itemlist]
            set selection [$itemlist selection]
            if {[llength $selection] < 1} {return}
            set index [lindex $selection 0]
            set attrList [lindex [$itemlist itemcget $index -values] 1]
            set descr    [lindex [$itemlist itemcget $index -values] 0]
            $_itemDialog itemconfigure 0 -text {Update}
            $i_nameLE configure -text "[getFromAttrList name $attrList]"
            $i_descrLE configure -text "$descr"
            $i_xLE configure -text [getFromAttrList X $attrList 0]
            $i_yLE configure -text [getFromAttrList Y $attrList 0]
            $i_imfileFE configure -text [getFromAttrList imfile $attrList]
            $i_sheetFileFE configure -text [getFromAttrList sheet $attrList]
            set ans [$_itemDialog draw]
            if {$ans == 1} {return}
            insertOrReplaceInAttrList name "[$i_nameLE cget -text]" attrList
            set descr "[$i_descrLE cget -text]"
            insertOrReplaceInAttrList X "[$i_xLE cget -text]" attrList
            insertOrReplaceInAttrList Y "[$i_yLE cget -text]" attrList
            set needmediatreeupdated no
            insertOrReplaceInAttrList imfile [$self _filewidgethandler "[$i_imfileFE cget -text]"] attrList
            insertOrReplaceInAttrList sheet [$self _filewidgethandler "[$i_sheetFileFE cget -text]"] attrList
            $itemlist itemconfigure $index -values [list $values $attrList]
            $itemlist itemconfigure $index -text $descr
            $self redrawspace
            set isdirty yes
            if {$needmediatreeupdated} {$options(-leveleditor) updatemediatree}
        }
        method _deleteitem {} {
            ## Delete an item.
            
            set itemlist [$spaceframe getElementWidgetById itemlist]
            set selection [$itemlist selection]
            if {[llength $selection] < 1} {return}
            set index [lindex $selection 0]
            set attrList [lindex [$itemlist itemcget $index -values] 1]
            set descr    [lindex [$itemlist itemcget $index -values] 0]
            if {[tk_messageBox -type yesno -default no -icon question -parent $win \
                 -message "Really delete $descr?"]} {
                $itemlist delete $index
                set isdirty yes
                $self redrawspace
            }
        }
        method _addnewexit {} {
            ## Add a new exit.
            
            set exitlist [$spaceframe getElementWidgetById exitlist]
            $_exitDialog itemconfigure 0 -text {Create}
            set ans [$_exitDialog draw]
            if {$ans == 1} {return}
            set attrList [list]
            insertOrReplaceInAttrList name "[$e_nameLE cget -text]" attrList
            set descr "[$e_descrLE cget -text]"
            insertOrReplaceInAttrList X "[$e_xLE cget -text]" attrList
            insertOrReplaceInAttrList Y "[$e_yLE cget -text]" attrList
            set needmediatreeupdated no
            insertOrReplaceInAttrList imfile [$self _filewidgethandler "[$e_imfileFE cget -text]"] attrList
            insertOrReplaceInAttrList sheet [$self _filewidgethandler "[$e_sheetFileFE cget -text]"] attrList
            set spacename "[$e_otherSpaceNameE cget -text]"
            if {"$spacename" eq ""} {return}
            insertOrReplaceInAttrList otherspace "$spacename" attrList
            #      puts stderr "*** $self _addnewexit: attrList = $attrList"
            $exitlist insert end -text "$descr" -values [list $descr "$attrList"]
            $self redrawspace
            set isdirty yes
            if {$needmediatreeupdated} {$options(-leveleditor) updatemediatree}
        }
        method _editexit {} {
            ## Edit an exit.
            
            set exitlist [$spaceframe getElementWidgetById exitlist]
            set selection [$exitlist selection]
            if {[llength $selection] < 1} {return}
            set index [lindex $selection 0]
            set attrList [lindex [$exitlist itemcget $index -values] 1]
            set descr    [lindex [$exitlist itemcget $index -values] 0]
            $_exitDialog itemconfigure 0 -text {Update}
            $e_nameLE configure -text "[getFromAttrList name $attrList]"
            $e_descrLE configure -text "$descr"
            $e_xLE configure -text "[getFromAttrList X $attrList]"
            $e_yLE configure -text "[getFromAttrList Y $attrList]"
            $e_imfileFE configure -text "[getFromAttrList imfile $attrList]"
            $e_sheetFileFE configure -text "[getFromAttrList sheet $attrList]"
            $e_otherSpaceNameE configure -text "[getFromAttrList otherspace $attrList]"
            set ans [$_exitDialog draw]
            if {$ans == 1} {return}
            set attrList [list]
            insertOrReplaceInAttrList name "[$e_nameLE cget -text]" attrList
            set descr "[$e_descrLE cget -text]"
            insertOrReplaceInAttrList X "[$e_xLE cget -text]" attrList
            insertOrReplaceInAttrList Y "[$e_yLE cget -text]" attrList
            set needmediatreeupdated no
            insertOrReplaceInAttrList imfile [$self _filewidgethandler "[$e_imfileFE cget -text]"] attrList
            insertOrReplaceInAttrList sheet [$self _filewidgethandler "[$e_sheetFileFE cget -text]"] attrList
            set spacename "[$e_otherSpaceNameE cget -text]"
            if {"$spacename" eq ""} {return}
            insertOrReplaceInAttrList otherspace "$spacename" attrList
            #puts stderr "*** $self _editexit: attrList = $attrList"
            $exitlist itemconfigure $index -text "$descr" 
            $exitlist itemconfigure $index -values [list $descr "$attrList"]
            $self redrawspace
            set isdirty yes
            if {$needmediatreeupdated} {$options(-leveleditor) updatemediatree}
        }
        method _deleteexit {} {
            ## Delete an exit.
            
            set exitlist [$spaceframe getElementWidgetById exitlist]
            set selection [$exitlist selection]
            if {[llength $selection] < 1} {return}
            set index [lindex $selection 0]
            set attrList [lindex [$exitlist itemcget $index -values] 1]
            set descr    [lindex [$exitlist itemcget $index -values] 0]
            if {[tk_messageBox -type yesno -default no -icon question -parent $win \
                 -message "Really delete $descr?"]} {
                $exitlist delete $index
                set isdirty yes
                $self redrawspace
            }
        }
        method _setspaceposition {} {
            ## Set space position.
            
            set XLE [$spaceframe getElementWidgetById X]
            set YLE [$spaceframe getElementWidgetById Y]
            $options(-leveleditor) setspaceposition $XLE $YLE $options(-spacefile)
        }
    }
}

## @}

package provide RPGMapLevelSpace 1.0

