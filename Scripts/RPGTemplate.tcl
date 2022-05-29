#* 
#* ------------------------------------------------------------------
#* Role PlayingDB V3.0 by Deepwoods Software
#* ------------------------------------------------------------------
#* RPGTemplate.tcl - Template Editor code
#* Created by Robert Heller on Sun Aug 30 18:14:10 2009
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
#package require BWLabelComboBox
package require LabelFrames
package require RPGUtilities
package require pdf4tcl

namespace eval RolePlayingDB3 {
    ##
    snit::widget Template {
        ## Template editor widget.
        option -template -readonly yes -default {}	  ;# Not used
        typevariable defaultfilename "template.rpgtmpl"
        ## @privatesection Default file name.
        typevariable middlebuttonPress {}
        ## Middle button press sequence.
        typevariable middlebuttonRelease {}
        ## Middle button release sequence.                                        
        typevariable rightbuttonRelease {}
        ## Right button release sequence.
        variable currentFilename
        ## Current filename.
        variable currentBaseFilename
        ## Current base filename.
        variable isdirty no
        ## Dirty flag
        variable path
        ## Path (mount point)
        variable tempfile
        ## Backing file.
        variable currentTemplateName
        ## Current template name.
        component banner
        ## Banner.
        component toolbar
        ## Toolbar.
        component panes
        ## Panes.
        component  sbpane
        ## Sidebar panee.
        component   sidebartree
        ## Sidebar tree.
        component  tmppane
        ## Template pane
        component   templatename
        ## Template name.
        component   templatesw
        ## Template Scroll window.
        component     templatetree
        ## Template tree.
        
        component _addNewTemplateDialog
        ## Add new template dialog.
        component   newTemplateName
        ## New template name.
        component   newTemplateClass
        ## New template class.
        
        component _addNewFieldDialog
        ## Add new field dialog.
        component   newFieldName
        ## New Field name.
        component   newFieldType
        ## New Field type.
        component   newFieldGenerator
        ## New Field generator.
        component   newFieldUpdatable
        ## New Field updatable?
        
        component _editFieldDialog
        ## Edit field dialog.
        component   theFieldName
        ## Field name.
        component   theFieldType
        ## Field type.
        component   theFieldGenerator
        ## Field generator.
        component   theFieldUpdatable
        ## Field updatable?
        
        component _editContainerTextDialog
        ## Edit container text dialog.
        component   containerText
        ## Container text.
        
        typevariable filetypes {
            {{Tempate Files} {.rpgtmpl}       }
            {{All Files}        *             }
        }
        ## File types.
        typemethod myfiletypes {} {
            ## Return my file types.
            # @returns file type structure.
            
            return $filetypes
        }
        typecomponent _editDialog
        ## Edit dialog.
        typevariable bannerImage
        ## Banner image.
        typevariable bannerBackground #eadc9b
        ## Banner background color.
        typevariable templateMonster
        ## Template monster.
        typeconstructor {
            ## One time initialization.
            
            set bannerImage [image create photo \
                             -file [file join $::RolePlayingDB3::ImageDir \
                                    TemplateBanner.png]]
            set templateMonster [image create photo \
                                 -file [file join $::RolePlayingDB3::ImageDir \
                                        SmallTemplateMonster.png]]
            set _editDialog {}
            switch $::tcl_platform(platform) {
                unix {
                    set middlebuttonPress <ButtonPress-2>
                    set middlebuttonRelease <ButtonRelease-2>
                }
                default {
                    set middlebuttonPress <Alt-ButtonPress-1>
                    set middlebuttonRelease <Alt-ButtonRelease-1>
                }
            }
            switch $::tcl_platform(platform) {
                unix -
                windows {
                    set rightbuttonRelease <ButtonRelease-3>
                }
                macintosh {
                    set rightbuttonRelease <Control-ButtonRelease-1>
                }
            }
        }
        typemethod _createEditDialog {} {
            ## Create the edit dialog.
            
            if {"$_editDialog" ne "" && [winfo exists "$_editDialog"]} {return}
            set _editDialog [Dialog .editTemplateDialog -image $templateMonster \
                             -cancel 2 -default 0 -modal local \
                             -parent . -side bottom \
                             -title "Edit Template" \
                             -transient yes]
            $_editDialog add new    -text {Create}
            $_editDialog add open   -text {Open}
            $_editDialog add cancel -text {Cancel}
            pack [message [$_editDialog getframe].message \
                  -text "Create a new Template file or\nopen an existing Template file?" \
                  -aspect 500] -fill both
        }
        typemethod edit {} {
            ## @publicsection Edit an existing template bundle.
            
            $type _createEditDialog
            set answer [$_editDialog draw]
            switch $answer {
                0 {$type new}
                1 {$type open}
                2 {return}
            }      
        }
        method getfile {} {
            ## Get the current filename.
            # @returns the current filename.
            
            return "$currentFilename"
        }
        typemethod new  {args} {
            ## Create a new template bundle.
            # @param ... Options:
            # @arg -like Window to get a default filename from.

            set like [from args -like {}]
            if {"$like" ne "" && [winfo exists "$like"]} {
                set like_filename [$like getfile]
            } else {
                set like_filename $defaultfilename
            }
            set newTop [RolePlayingDB3::RPGToplevel .template%AUTO% \
			-mainframeconstructor $type -mainframetemplate {} \
			-class TemplateEditor]
            $newTop opennew
        }
        method opennew {} {
            ## Open a new template bundle.
            
            set currentFilename {}
            set currentBaseFilename *noname*
            $sidebartree configure -label "$currentBaseFilename"
            set path [$type genname]
            set tempfile [file join $::RolePlayingDB3::TmpDir $path]
            while {[file exists $tempfile]} {
                set path [$type genname]
                set tempfile [file join $::RolePlayingDB3::TmpDir $path]
            }
            vfs::rpg::Mount $tempfile /$path
            foreach theclass {Character Dressing Monster Spell Treasure TrickTrap} {
                file mkdir [file join /$path $theclass]
                close [open [file join /$path $theclass flag] w]
            }
            $sidebartree configure -directory [file join /$path]
            [winfo toplevel $win] configure -title "Template Edit: $currentBaseFilename"
        }
        method new {} {
            ## Create a new template bundle.
            
            $type new -like $win
        }
        typevariable genindex 0
        ## @privatesection Symbol generation index.
        typemethod genname {} {
            ## Generate a new unique symbol.
            # @returns the new unique symbol.
            
            incr genindex
            return [format {TEMPLATE%05d} $genindex]
        }
        typemethod open {args} {
            ## @publicsection Open an existing template bundle.
            # @param ... Options:
            # @arg -like Editor to create a template bundle like.
            
            set like [from args -like {}]
            if {"$like" ne "" && [winfo exists "$like"]} {
                set like_filename [$like getfile]
                set w $like
            } else {
                set like_filename $defaultfilename
                set w .
            }
            set currentFilename [tk_getOpenFile -defaultextension .rpgtmpl \
                                 -filetypes $filetypes \
                                 -initialdir [file dirname $like_filename] \
                                 -initialfile $like_filename \
                                 -parent $w \
                                 -title "File to open"]
            if {"$currentFilename" eq ""} {return}
            set newTop [RolePlayingDB3::RPGToplevel .template%AUTO% \
			-mainframeconstructor $type -mainframetemplate {} \
			-class TemplateEditor]
            $newTop openold "$currentFilename"
        }
        typemethod openfile {filename} {
            ## Open an existing template bundle file.
            # @param filename Name of the file to open.
            
            set currentFilename "$filename"
            set newTop [RolePlayingDB3::RPGToplevel .template%AUTO% \
			-mainframeconstructor $type -mainframetemplate {} \
			-class TemplateEditor]
            $newTop openold "$currentFilename"
        }
        proc _copyTemplateTreeHelper {srcfile destdir} {
            ## @privatesection Recursivly copy a template bundle to a temp
            # file system.
            # @param srcfile Source file or directory.
            # @param destdir Destination directory.
            
            #puts stderr "*** _copyTemplateTreeHelper $srcfile $destdir"
            file stat $srcfile fssrc
            #puts stderr "*** _copyTemplateTreeHelper: $srcfile $fssrc(size) bytes, type: $fssrc(type)"
            set filename [file tail $srcfile]
            if {$fssrc(type) ne "directory"} {
                file copy $srcfile $destdir
                file stat [file join $destdir $filename] fs
                #puts stderr "*** _copyTemplateTreeHelper: [file join $destdir $filename]: $fs(size) bytes"
            } else {
                set files [glob -nocomplain [file join $srcfile *]]
                file mkdir [file join $destdir $filename]
                foreach f $files {
                    _copyTemplateTreeHelper $f [file join $destdir $filename]
                }
            }
        }
        
        method openold {_filename} {
            ## @publicsection Open an existing template bundle file.
            # @param _filename Name of the file to open.
            
            set path [$type genname]
            set tempfile [file join $::RolePlayingDB3::TmpDir $path]
            while {[file exists $tempfile]} {
                set path [$type genname]
                set tempfile [file join $::RolePlayingDB3::TmpDir $path]
            }
            vfs::rpg::Mount $tempfile /$path
            set currentFilename $_filename
            set currentBaseFilename [file tail $currentFilename]
            $sidebartree configure -label "$currentBaseFilename"
            set inpath [$type genname]
            vfs::zip::Mount $currentFilename $inpath
            foreach classDir {Character Dressing Monster Spell Treasure TrickTrap} {
                _copyTemplateTreeHelper [file join $inpath $classDir] /$path
            }
            
            #      puts stderr "*** $self openold: files in $currentFilename ($inpath): [glob -nocomplain $inpath/*]"
            #      puts stderr "*** $self openold: files in $tempfile (/$path): [glob -nocomplain /$path/*]"
            vfs::unmount $inpath
            $sidebartree configure -directory [file join /$path]
            [winfo toplevel $win] configure -title "Template Edit: $currentBaseFilename"
        }
        method rescantemplates {} {
            ## Rescan the template tree.
            $sidebartree redrawdirtree
        }
        method open {args} {
            ## Open an existing template bundle.
            $type open -like $win {*}$args
        }
        method save {} {
            ## Save the current template bundle in the current file.
            
            #      puts stderr "*** $self save: currentFilename = $currentFilename"
            $self saveas $currentFilename
        }
        method saveas {{_filename {}}} {
            ## Save the current template bundle in specified file.
            # @param _filename File name to save to.
            
            if {"$_filename" eq {}} {
                set _filename [tk_getSaveFile -defaultextension .rpgtmpl \
                               -filetypes $filetypes \
                               -initialdir [file dirname $currentFilename] \
                               -initialfile $currentFilename \
                               -parent $win \
                               -title "Save As File"]
            }
            if {"$_filename" eq {}} {return}
            ::ZipArchive createZipFromDirtree $_filename /$path \
                  -comment "RPGV3 Template Bundle"
            set isdirty no
            if {"$currentFilename" ne "$_filename"} {
                set currentFilename $_filename
                set currentBaseFilename [file tail $currentFilename]
                $sidebartree configure -label "$currentBaseFilename"
                [winfo toplevel $win] configure -title "Template Edit: [file tail $currentFilename]"
            }
        }
        method print {} {
            ## Print (to a PDF file) the current template bundle.
            
            set printfile "[file rootname $currentFilename].pdf"
            if {"$printfile" eq ".pdf"} {set printfile "Template.pdf"}
            set pdfobj [::RolePlayingDB3::PrintDialog drawPrintDialog \
                        -parent $win \
                        -what Template \
                        -filename $printfile]
            if {"$pdfobj" eq ""} {return}
            if {"$currentFilename" eq ""} {
                set heading [file tail "$currentBaseFilename"]
            } else {
                set heading [file tail "$currentFilename"]
            }
            $self outputXMLToPDF $pdfobj $heading
            ::RolePlayingDB3::PrintDialog printprogress end      
            $pdfobj destroy
        }
        variable pageno 0
        ## @privatesection Current page number.
        variable lineno 1000    
        ## Current line number.
        method outputXMLToPDF {pdfobj {heading "Sheet"}} {
            ## @publicsection Output XML to a PDF file.
            # @param pdfobj The PDF object to print to.
            # @param heading The heading.
            
            $pdfobj setFont 10 Courier
            $pdfobj setLineSpacing [expr {14.0 / 10.0}]
            set pageno 0
            set lineno 1000
            $self _outputXMLToPDF_processNodesAt $heading {} {} $pdfobj
        }
        method newPDFPage {pdfobj heading subheading} {
            ## Start a new page.
            # @param pdfobj The PDF object to print to.
            # @param heading The heading.
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
        method _outputXMLToPDF_processNodesAt {heading subheading node pdfobj {indent 0}} {
            ## @privatesection Helper function to print one node.
            # @param heading The heading.
            # @param subheading The subheading.
            # @param node The current XML node.
            # @param pdfobj The PDF object to print to.
            # @param indent The current indentation.
            
            foreach n [$templatetree children "$node"] {
                if {[string is integer -strict $n]} {
                    set subheading "[$templatetree item "$n" -values]"
                    set dheight [expr {[lindex [$pdfobj getDrawableArea] 1] - ($lineno * 14)}]
                    #	  puts stderr "*** $self _outputXMLToPDF_processNodesAt: pageno = $pageno, lineno = $lineno"
                    if {$pageno < 1 || $dheight < 28} {
                        $self newPDFPage $pdfobj "$heading" "$subheading"
                    }
                    $pdfobj setFont 10 Courier-Bold
                    $pdfobj text "$subheading" -x $indent
                    $pdfobj newLine
                    incr lineno
                    #	  puts stderr "*** $self _outputXMLToPDF_processNodesAt: lineno = $lineno (after Container)"
                    $pdfobj setFont 10 Courier
                } else {
                    set text [$templatetree item "$n" -text]
                    set dheight [expr {[lindex [$pdfobj getDrawableArea] 1] - ($lineno * 14)}]
                    #	  puts stderr "*** $self _outputXMLToPDF_processNodesAt: pageno = $pageno, lineno = $lineno"
                    if {$pageno < 1 || $dheight < 28} {
                        $self newPDFPage $pdfobj "$heading" "$subheading"
                    }
                    $pdfobj text "$text" -x $indent
                    $pdfobj newLine
                    incr lineno
                    $self _outputXMLToPDF_processNodesAt "$heading" "$subheading" $n $pdfobj [expr {$indent + 24}]
                }
            }
        }
        variable templateFile {}
        ## Template file.
        variable parsedxml {}
        ## Parsed XML DOM Tree.
        variable _xmlnodes -array {}
        ## Map from tree nodes to XML DOM nodes.
        variable _treenodes -array {}
        ## Map from XML DOM nodes to tree nodes.
        variable _datanodes -array {}
        ## Map from XML DOM nodes to data nodes.
        method close {args} {
            ## @publicsection Close the current bundle file.
            # @param ... Options: none.
            
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
            if {$parsedxml ne {}} {
                array unset _xmlnodes
                array unset _treenodes
                array unset _datanodes
                catch {$parsedxml destroy}
                set parsedxml {}
            }
            destroy [winfo toplevel $win]
        }
        constructor {args} {
            ## Construct a template bundle editor widget.
            # @param ... Options:
            # @arg -template Not used.
            
            install banner using ttk::label $win.banner -image $bannerImage -anchor w \
                  -background $bannerBackground
            pack $banner -fill x
            install toolbar using ButtonBox $win.toolbar -orient horizontal
            pack $toolbar -fill x
            $toolbar add ttk::button addtemp -text {Add Template} \
                  -command [mymethod _addtemplate]
            $toolbar add ttk::button deltemp -text {Delete Template}  \
                  -command [mymethod _deletetemplate]
            $toolbar add ttk::button addfield -text {Add Field or Container} \
                  -command [mymethod _addfield]
            $toolbar add ttk::button edittext -text {Edit Container Text} \
                  -command [mymethod _edittext]
            $toolbar add ttk::button delfield -text {Delete Field or Container} \
                  -command [mymethod _deletefield]
            $toolbar add ttk::button editfield -text {Edit Field} \
                  -command [mymethod _editfield]
            install panes using ttk::panedwindow $win.panes -orient horizontal
            pack $panes -fill both -expand yes
            install sidebartree using ::RolePlayingDB3::LabeledDirTree $panes.sidebartree \
                  -showextension no -filepattern *.xml
            $panes add $sidebartree -weight 1
            $sidebartree binditem file <Double-Button-1> [mymethod _openTemplate]
            set tmppane [ttk::frame $panes.tmppane]
            $panes add $tmppane -weight 5
            install templatename using LabelEntry $tmppane.templatename \
                  -editable no -label "Open Template:" \
                  -textvariable [myvar currentTemplateName]
            pack $templatename -fill x
            install templatesw using ScrolledWindow $tmppane.templatesw \
                  -scrollbar both -auto both
            pack $templatesw -fill both -expand yes
            install templatetree using ttk::treeview \
                  [$templatesw getframe].templatetree \
                  -columns {tag attlist} \
                  -displaycolumns {} \
                  -show tree \
                  -selectmode browse
            $templatesw setwidget $templatetree
            $templatetree tag bind item $rightbuttonRelease  [mymethod _postItemMenu %x %y]
            $templatetree tag bind item $middlebuttonPress   [mymethod _startMove %x %y]
            $templatetree tag bind item $middlebuttonRelease [mymethod _endMove %x %y]
            $self configurelist $args
            
            update
            install _addNewTemplateDialog using Dialog [winfo toplevel $win].addNewTemplateDialog \
                  -image $templateMonster \
                  -cancel 1 -default 0 -modal local \
                  -parent $win -side bottom \
                  -title "Add New Template" \
                  -transient yes
            $_addNewTemplateDialog add add    -text {Add}
            $_addNewTemplateDialog add cancel -text {Cancel}
            set dframe [$_addNewTemplateDialog getframe]
            install newTemplateName using LabelEntry $dframe.newTemplateName \
                  -label "Name:" -labelwidth 6
            pack $newTemplateName -fill x
            install newTemplateClass using LabelComboBox $dframe.newTemplateClass \
                  -label "Class:" -labelwidth 6 \
                  -values {Character Monster Spell 
                Treasure TrickTrap Dressing} \
                  -editable no
            pack $newTemplateClass -fill x
            $newTemplateClass set [lindex [$newTemplateClass cget -values] 0]
            
            #update
            install _addNewFieldDialog using Dialog [winfo toplevel $win].addNewFieldDialog \
                  -image $templateMonster \
                  -cancel 1 -default 0 -modal local \
                  -parent $win -side bottom \
                  -title "Add New Field" \
                  -transient yes
            $_addNewFieldDialog add add    -text {Add}
            $_addNewFieldDialog add cancel -text {Cancel}
            set dframe [$_addNewFieldDialog getframe]
            install newFieldName using LabelEntry $dframe.newFieldName \
                  -label "Name:" -labelwidth 10
            pack $newFieldName -fill x
            install newFieldType using LabelComboBox $dframe.newFieldType \
                  -label "Type:" -labelwidth 10 \
                  -editable no \
                  -values {{Whole Number} {Word / Short Phrase}
                {Long Text} {Graphic} {Document}
                {Container}}
            pack $newFieldType -fill x
            $newFieldType set [lindex [$newFieldType cget -values] 0]
            install newFieldGenerator using LabelComboBox $dframe.newFieldGenerator \
                  -label "Generator:" -labelwidth 10 \
                  -editable yes \
                  -values {{} d% 1d4 2d4 3d4 5d4 1d6 2d6 3d6 4d6
                1d8 2d8 3d8 4d8 5d8 1d10 2d10 3d10
                1d12 2d12 3d12 1d20 2d20}
            pack $newFieldGenerator -fill x
            $newFieldGenerator set [lindex [$newFieldGenerator cget -values] 0]
            install newFieldUpdatable using LabelComboBox $dframe.newFieldUpdatable \
                  -label "Updatable:" -labelwidth 10 \
                  -editable no \
                  -values {yes no}
            pack $newFieldUpdatable -fill x
            $newFieldUpdatable set [lindex [$newFieldUpdatable cget -values] 0]
            
            install _editFieldDialog using Dialog [winfo toplevel $win].editFieldDialog \
                  -image $templateMonster \
                  -cancel 1 -default 0 -modal local \
                  -parent $win -side bottom \
                  -title "Edit Field" \
                  -transient yes
            $_editFieldDialog add add    -text {Update}
            $_editFieldDialog add cancel -text {Cancel}
            set dframe [$_editFieldDialog getframe]
            install theFieldName using LabelEntry $dframe.theFieldName \
                  -label "Name:" -labelwidth 10 \
                  -editable no
            pack $theFieldName -fill x
            install theFieldType using LabelComboBox $dframe.theFieldType \
                  -label "Type:" -labelwidth 10 \
                  -editable no \
                  -values {{Whole Number} {Word / Short Phrase}
                {Long Text} {Graphic} {Document}}
            pack $theFieldType -fill x
            $theFieldType set [lindex [$theFieldType cget -values] 0]
            install theFieldGenerator using LabelComboBox $dframe.theFieldGenerator \
                  -label "Generator:" -labelwidth 10 \
                  -editable yes \
                  -values {{} d% 1d4 2d4 3d4 5d4 1d6 2d6 3d6 4d6
                1d8 2d8 3d8 4d8 5d8 1d10 2d10 3d10
                1d12 2d12 3d12 1d20 2d20}
            pack $theFieldGenerator -fill x
            $theFieldGenerator set [lindex [$theFieldGenerator cget -values] 0]
            install theFieldUpdatable using LabelComboBox $dframe.theFieldUpdatable \
                  -label "Updatable:" -labelwidth 10 \
                  -editable no \
                  -values {yes no}
            pack $theFieldUpdatable -fill x
            $theFieldUpdatable set [lindex [$theFieldUpdatable cget -values] 0]
            
            #update
            install _editContainerTextDialog using \
                  Dialog [winfo toplevel $win].editContainerTextDialog \
                  -image $templateMonster \
                  -cancel 1 -default 0 -modal local \
                  -parent $win -side bottom \
                  -title "Edit container text" \
                  -transient yes
            $_editContainerTextDialog add update -text {Update}
            $_editContainerTextDialog add cancel -text {Cancel}
            set dframe [$_editContainerTextDialog getframe]
            install containerText using LabelEntry $dframe.containerText \
                  -label "Text:" -labelwidth 5
            pack $containerText -fill x
            update idle
        }
        method _addtemplate {} {
            ## @privatesection Add a new template to the bundle.
            
            #      puts stderr "*** $self _addtemplate"
            #      puts stderr "*** $self _addtemplate: _addNewTemplateDialog = $_addNewTemplateDialog"
            switch [$_addNewTemplateDialog draw] {
                0 {
                    set name [$newTemplateName cget -text]
                    set class [$newTemplateClass cget -text]
                    if {[file extension $name] eq ""} {append name ".xml"}
                    set filename [file join /$path $class $name]
                    if {[file exists $filename]} {
                        tk_messageBox -type ok -icon warning -parent $win \
                              -message "A Template named $name already exists! Please select a different name or delete the existing template."
                        return
                    }
                    set fp [open "$filename" w]
                    puts $fp {<?xml version="1.0" ?>}
                    puts $fp "<rpgv3:$class name=\"$class\" xmlns:rpgv3=\"http://www.deepsoft.com/roleplayingdb/v3xmlns\">"
                    puts $fp "</rpgv3:$class>"
                    close $fp
                    $self rescantemplates
                    set isdirty yes
                }
                1 {return}
            }
        }
        method _deletetemplate {} {
            ## Delete a template from the bundle.
            
            set selected [$sidebartree selection get]
            switch -- [llength $selected] {
                0 {
                    tk_messageBox -parent $win -type ok -icon info -message "Please select a template to delete!"
                    return
                }
                1 {
                    set thetemplate "[lindex $selected 0]"
                    set ans [tk_messageBox -parent $win -type yesno -icon question -message "Are you sure you want to delete $thetemplate?"]
                    if {"$ans" ne "yes"} {return}
                    file delete [$sidebartree itemcget $thetemplate -fullpath]
                    $self rescantemplates
                    set isdirty yes
                }
                default {
                    tk_messageBox -parent $win -type ok -icon info -message "Please select only one template to delete!"
                }
            }
        }
        method _openTemplate {thetemplate} {
            ## Open a template file.
            # @param thetemplate The template file to open.
            
            set templateFile [$sidebartree itemcget $thetemplate -fullpath]
            regsub "/$path/" [file rootname "$templateFile"] {} currentTemplateName
            if {[catch {open $templateFile r} fp]} {
                error "_openTemplate: open $templateFile r: $fp"
                return
            }
            set xml [read $fp]
            close $fp
            if {$parsedxml ne {}} {
                array unset _xmlnodes
                array unset _treenodes
                array unset _datanodes
                catch {$parsedxml destroy}
                set parsedxml {}
            }
            $templatetree delete [$templatetree children {}]
            set parsedxml [ParseXML create %AUTO% $xml]
            $self _populate_templatetree [lindex [$parsedxml children] 0] {}
        }
        method _populate_templatetree {node treenode} {
            ## Populate the tree view from the XML DOM tree.
            # @param node XML DOM node.
            # @param treenode Parent tree node.
            
            set tag [$node cget -tag]
            set attlist [$node cget -attributes]
            set data [string trim [$node data]]
            set nodename $tag
            if {"$tag" eq "Field"} {
                foreach {n v} $attlist {
                    if {"$n" eq "name"} {set nodename "Field:$v"}
                }
                set open no
            } else {
                set open yes
            }
            set text "$nodename"
            foreach {n v} $attlist {
                if {"$n" ne "name" && "$v" ne ""} {
                    append text " $n=\"$v\""
                }
            }
            regsub -all {[[:space:]]} "$nodename" {_} nodename
            set newtreenode [$templatetree insert $treenode end -text $text \
                             -tag item -values [list $tag $attlist] -open $open]
            set _xmlnodes($newtreenode) $node
            set _treenodes($node) $newtreenode
            if {"$tag" ne "Field"} {
                set datanode [$templatetree insert $newtreenode end -text $data \
                              -values [list $data {}] -open no]
                set _datanodes($node) $datanode
            }
            foreach c [$node children] {
                $self _populate_templatetree $c $newtreenode
            }
        }
        method _addfield {{item {}} {menu {}}} {
            ## Add a field.
            # @param item Tree container node to add the field to. If empty,
            # get node from the tree selection.
            # @param menu Popup context menu to be unposted and destroyed.
            
            if {"$item" ne ""} {
                set selected [list $item]
            } else {
                set selected [$templatetree selection]
            }
            if {"$menu" ne ""} {
                catch {$menu unpost}
                catch {$menu destroy}
            }
            switch -- [llength $selected] {
                0 {
                    tk_messageBox -parent $win -type ok -icon info -message "Please select a parent field to add a child field to!"
                    return
                }
                1 {
                    set parent [lindex $selected 0]
                    foreach {tag attrlist args} [$templatetree item "$parent" -values] {break}
                    if {"$tag" eq "Field"} {
                        tk_messageBox -parent $win -type ok -icon info -message "Fields cannot be used as Containers!"
                        return
                    }
                    if {[llength $attrlist] > 2} {
                        tk_messageBox -parent $win -type ok -icon info -message "Parent is not a properly formed container, it has too many attributes!"
                        return
                    }
                    if {[llength $attrlist] > 0 && [lindex $attrlist 0] ne "name"} {
                        tk_messageBox -parent $win -type ok -icon info -message "Parent does not have a name attribute!"
                        return
                    }
                    set ans [$_addNewFieldDialog draw]
                    switch -- $ans {
                        0 {
                            set name [$newFieldName cget -text]
                            set ftype [$newFieldType cget -text]
                            set generator [$newFieldGenerator cget -text]
                            set updatable [$newFieldUpdatable cget -text]
                            if {!$updatable && 
                                [lsearch -exact {{Whole Number} {Word / Short Phrase}} "$ftype"] < 0} {
                                tk_messageBox -type ok -icon warning -message "Only Whole Numbers and Word / Short Phrase type fields can be locked!"
                                set updatable yes
                            }
                            if {"$ftype" eq "Container" && "$name" ne "Field"} {
                                set attrlist [list name $name]
                                # Generate a tag that XML parsers will be happy with
                                regsub -all {[[:space:]]} "$name" {_} tag
                                regsub -all {:} $tag {-} tag
                                regsub -all {[<>&'"]} $tag {} tag;# get rid of special XML chars
                                              set tag [string totitle $tag]
                                              set nodename $tag
                            } else {
                                set attrlist [list name $name type $ftype \
                                              generator $generator \
                                              updatable $updatable]
                                set tag Field
                                set nodename "Field:$name"
                            }
                            set text "$nodename"
                            foreach {n v} $attrlist {
                                if {"$n" ne "name" && "$v" ne ""} {
                                    append text " $n=\"$v\""
                                }
                            }
                            regsub -all {[[:space:]]} "$nodename" {_} nodename
                            set newnode [$templatetree insert "$parent" end -text $text \
                                         -values [list $tag  $attrlist] \
                                         -open yes -tag item]
                            $templatetree see "$newnode"
                            set newxmlnode [SimpleDOMElement create %AUTO% \
                                            -tag $tag -attributes $attrlist \
                                            -opts [list -namespace \
                                                   http://www.deepsoft.com/roleplayingdb/v3xmlns]]
                            $_xmlnodes($parent) addchild $newxmlnode
                            set _xmlnodes($newnode) $newxmlnode
                            set _treenodes($newxmlnode) $newnode
                            if {$tag ne "Field"} {
                                set datanode [$templatetree insert $newnode end -text "" \
                                -values [list {} {}] -open no]
                                set _datanodes($newxmlnode) $datanode
                            }
                            $self _regenerateXMLFromTree
                        }
                        1 {return}
                    }
                }
                default {
                    tk_messageBox -parent $win -type ok -icon info -message "Please select only one parent container to add a child field to!"
                    return
                }
            }
        }
        method _regenerateXMLFromTree {} {
            ## Regenerate XML data from the tree.
            
            foreach item [array names _xmlnodes] {
                set node $_xmlnodes($item)
                lassign [$templatetree item $item -values] tag attributes
                $node configure -attributes $attributes
            }
            foreach node [array names _datanodes] {
                set item $_datanodes($node)
                $node setdata [$templatetree item $item -text]
            }
            if {[catch {open $templateFile w} fp]} {
                error "_regenerateXMLFromTree: open $templateFile w: $fp"
                return
            }
            puts $fp {<?xml version="1.0" ?>}
            $parsedxml displayTree $fp {} -addnamespace yes
            close $fp
            set isdirty yes
        }
        method _deletefield {{item {}} {menu {}}} {
            ## Delete a field.
            # @param item Tree item to delete. If empty, get node from the 
            # tree selection.
            # @param menu Popup context menu to be unposted and destroyed.
            
            if {"$item" ne ""} {
                set selected [list $item]
            } else {
                set selected [$templatetree selection]
            }
            if {"$menu" ne ""} {
                catch {$menu unpost}
                catch {$menu destroy}
            }
            switch -- [llength $selected] {
                0 {
                    tk_messageBox -parent $win -type ok -icon info -message "Please select a field or container to delete!"
                    return
                }
                1 {
                    set thefield "[lindex $selected 0]"
                    if {[$templatetree parent "$thefield"] eq ""} {
                        tk_messageBox -parent $win -type ok -icon warning -message "This is the toplevel container and cannot be deleted!"
                        return
                    }
                    if {[llength [$templatetree children "$thefield"]] > 1} {
                        tk_messageBox -parent $win -type ok -icon info -message "This container is not empty, please be very sure you want to delete it!"
                    }
                    set ans [tk_messageBox -parent $win -type yesno -icon question -message "Are you sure you want to delete [$_xmlnodes($thefield) attribute name]?"]
                    if {"$ans" ne "yes"} {return}
                    set node $_xmlnodes($thefield)
                    set parent $_xmlnodes([$templatetree parent "$thefield"])
                    $self _clearmaps $node
                    $parent removeChild $node
                    $node destroy
                    $templatetree delete "$thefield"
                    $self _regenerateXMLFromTree
                }
                default {
                    tk_messageBox -parent $win -type ok -icon info -message "Please select only one field or container to delete!"
                }
            }
        }
        method _clearmaps {node} {
            ## Clear the maps for a given node.
            # @param node The XML node.
            
            #puts stderr "*** ${self}::_clearmaps $node"
            set thefield $_treenodes($node)
            #puts stderr "*** ${self}::_clearmaps: thefield is $thefield"
            unset _xmlnodes($thefield)
            unset _treenodes($node)
            catch {unset _datanodes($node)}
            foreach c [$node children] {  
                $self _clearmaps $c
            }
        }
        method _edittext {{item {}} {menu {}}} {
            ## Edit the text of a container.
            # @param item Tree item to edit. If empty, get node from the tree 
            # selection.
            # @param menu Popup context menu to be unposted and destroyed.
            
            if {"$item" ne ""} {
                set selected [list $item]
            } else {
                set selected [$templatetree selection]
            }
            if {"$menu" ne ""} {
                catch {$menu unpost}
                catch {$menu destroy}
            }
            switch -- [llength $selected] {
                0 {
                    tk_messageBox -parent $win -type ok -icon info -message "Please select a container!"
                    return
                }
                1 {
                    set thecontainer "[lindex $selected 0]"
                    set node $_xmlnodes($thecontainer)
                    set datanode $_datanodes($node)
                    set t [$templatetree item $datanode -text]
                    set d [$templatetree item "$thecontainer" -values]
                    lassign $d tag attrlist
                    if {"$tag" eq "Field"} {
                        tk_messageBox -parent $win -type ok -icon info -message "Please select a container.  Text cannot be added a Field!"
                        return
                    }
                    $containerText configure -text [string trim "$t"]
                    set ans [$_editContainerTextDialog draw]
                    switch -exact $ans {
                        0 {
                            set newtext [string trim [$containerText get]]
                            $templatetree item $datanode -text $newtext
                            $self _regenerateXMLFromTree
                        }
                        1 {
                        }
                    }
                }
                default {
                    tk_messageBox -parent $win -type ok -icon info -message "Please select only one container to text edit!"
                }
            }
        }
        method _editfield {{item {}} {menu {}}} {
            ## Edit a field.
            # @param item Tree item to edit. If empty, get node from the tree 
            # selection.
            # @param menu Popup context menu to be unposted and destroyed.
            
            if {"$item" ne ""} {
                set selected [list $item]
            } else {
                set selected [$templatetree selection]
            }
            if {"$menu" ne ""} {
                catch {$menu unpost}
                catch {$menu destroy}
            }
            switch -- [llength $selected] {
                0 {
                    tk_messageBox -parent $win -type ok -icon info -message "Please select a field!"
                    return
                }
                1 {
                    set thefield "[lindex $selected 0]"
                    set t [$templatetree item "$thefield" -text]
                    set d [$templatetree item "$thefield" -values]
                    if {[string is integer -strict "$thefield"] && "$t" eq "$d"} {
                        tk_messageBox -parent $win -type ok -icon info -message "Please select a field.  Text cannot be directly edited!"
                        return
                    }
                    lassign $d tag attrlist
                    if {"$tag" ne "Field"} {
                        tk_messageBox -parent $win -type ok -icon info -message "Please select a field.  Only fields can be edited!"
                        return
                    }
                    $theFieldGenerator configure -text {}
                    foreach {n v} $attrlist {
                        switch $n {
                            name {$theFieldName configure -text "$v"}
                            type {$theFieldType configure -text "$v"}
                            generator {$theFieldGenerator configure -text "$v"}
                            updatable {$theFieldUpdatable configure -text "$v"}
                        }
                    }
                    set ans [$_editFieldDialog draw]
                    switch $ans {
                        1 {return}
                        0 {
                            set name [$theFieldName cget -text]
                            set ftype [$theFieldType cget -text]
                            set generator [$theFieldGenerator cget -text]
                            set updatable [$theFieldUpdatable cget -text]
                            if {!$updatable && [lsearch -exact {{Whole Number} {Word / Short Phrase}} "$ftype"] < 0} {
                                tk_messageBox -type ok -icon warning -message "Only Whole Numbers and Word / Short Phrase type fields can be locked!"
                                set updatable yes
                            }
                            set attrlist [list name $name  \
                                          type $ftype \
                                          generator $generator \
                                          updatable $updatable]
                            set text "Field:[$theFieldName cget -text]"
                            foreach {n v} $attrlist {
                                if {"$n" ne "name" && "$v" ne ""} {
                                    append text " $n=\"$v\""
                                }
                            }
                            $templatetree item "$thefield" -text $text -values [list $tag $attrlist]
                            $self _regenerateXMLFromTree
                        }
                    }
                }
                default {
                    tk_messageBox -parent $win -type ok -icon info -message "Please select only one field!"
                }
            }
        }
        method _postItemMenu {x y} {
            ## Post a popup menu on an item.
            # @param x Mouse X coordinate.
            # @param y Mouse Y coordinate.
            
            set item [$templatetree identify item $x $y]
            set postX [winfo pointerx $templatetree]
            set postY [winfo pointery $templatetree]
            regsub -all {[[:space:].]} "$item" {} mpath
            set mpath [string tolower $mpath]
            set mpath "$templatetree.$mpath"
            set basepath $mpath
            set count 0
            while {[winfo exists $mpath]} {
                incr count
                set mpath "$basepath$count"
            }
            set t [$templatetree item "$item" -text]
            set d [$templatetree item "$item" -values]
            #      puts stderr "*** $self _postItemMenu: item = $item, t = $t, d = $d"
            if {[string is integer -strict "$item"] && "$t" eq "$d"} {return};# Text
            lassign $d tag attrlist
            menu $mpath -tearoff no
            set iscontainer [expr {"$tag" ne "Field"}]
            #      puts stderr "*** $self _postItemMenu: iscontainer = $iscontainer, tag = $tag"
            if {$iscontainer} {
                $mpath add command -label "Add Field" -command [mymethod _addfield $item $mpath]
                $mpath add command -label "Edit Text" -command [mymethod _edittext $item $mpath]
            } else {
                $mpath add command -label "Edit Field" -command [mymethod _editfield $item $mpath]
            }
            $mpath add command -label "Delete"  -command [mymethod _deletefield $item $mpath]
            $mpath add command -label "Cancel"  -command [mymethod _dismisMenu $mpath]
            $mpath post $postX $postY
        }
        method _dismisMenu {menu} {
            ## Dismis a menu.
            # @param menu The menu to dismis.
            
            catch {$menu unpost}
            catch {$menu destroy}
        }
        variable fromX 
        ## Drag from X.
        variable fromY
        ## Drag from Y.
        variable oldCursor
        ## Old cursor.
        method _startMove {mx my} {
            ## Start draging.
            # @param mx Starting X.
            # @param my Starting Y.
            
            set fromX $mx
            set fromY $my
            set oldCursor [$templatetree cget -cursor]
            $templatetree configure -cursor crosshair
        }
        method _endMove {mx my} {
            ## End draging.
            # @param mx Ending X.
            # @param my Ending Y.
            
            set item [$templatetree identify item $fromX $fromY]
            if {"$item" eq ""} {
                return
            }
            
            set toX $mx
            set toY $my
            $templatetree configure -cursor $oldCursor
            if {$toY != $fromY} {
                #puts stderr "*** $self _endMove: toY = $toY, $fromY = $fromY"
                set which [$templatetree identify item $toX $toY]
                if {"$which" eq ""} {
                    tk_messageBox -type ok -icon info \
                          -message "Cannot move to nowhere"
                    return
                }
                #puts stderr "*** $self _endMove: which = $which, item = $item"
                if {$which eq $item} {return}
                set myparent [$templatetree parent $item]
                if {"$myparent" eq ""} {
                    tk_messageBox -type ok -icon info \
                          -message "Cannot move sheet class container"
                    return
                }
                
                set hisparent [$templatetree parent $which]
                #puts stderr "*** $self _endMove: myparent = $myparent, hisparent = $hisparent"
                if {"$hisparent" eq ""} {
                    tk_messageBox -type ok -icon info \
                          -message "Cannot move fields or containers above sheet class container"
                    return
                }
                if {$myparent eq $hisparent} {
                    set currentOrder [$templatetree children $hisparent]
                    set itemIndex [lsearch -exact $currentOrder $item]
                    set newOrder [lreplace $currentOrder $itemIndex $itemIndex]
                    set whichIndex [lsearch -exact $newOrder $which]
                    set newOrder [linsert $newOrder $whichIndex $item]
                    $templatetree children $hisparent $newOrder
                    set hisxmlparent $_xmlnodes($hisparent)
                    set hissibs [$hisxmlparent children]
                    set me $_xmlnodes($item)
                    set myindex [lsearch -exact $hissibs $me]
                    set newsibs [lreplace $hissibs $myindex $myindex]
                    set hisindex  [lsearch -exact $newsibs $_xmlnodes($which)]
                    set newsibs [linsert $newsibs $hisindex $me]
                    $hisxmlparent replaceChildren $newsibs
                } else {
                    $templatetree move $hisparent $item [$templatetree index $which]
                    set me $_xmlnodes($item)
                    $_xmlnodes($myparent) removeChild $me
                    set him $_xmlnodes($which)
                    set hisxmlparent $_xmlnodes($hisparent)
                    set hissibs [$hisxmlparent children]
                    set hisindex  [lsearch -exact $hissibs $him]
                    set newsibs [linsert $hissibs $hisindex $me]
                    $hisxmlparent replaceChildren $newsibs
                }
                $self _regenerateXMLFromTree
            }
        }
    }
}

## @}

package provide RPGTemplate 1.0
