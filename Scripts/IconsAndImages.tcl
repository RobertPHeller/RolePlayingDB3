#*
#*
#*  System        : 
#*  Module        : 
#*  Object Name   : $RCSfile$
#*  Revision      : $Revision$
#*  Date          : $Date$
#*  Author        : $Author$
#*  Created By    : Robert Heller
#*  Created       : Thu May 16 13:48:41 2013
#*  Last Modified : <220523.1628>
#*
#*  Description	
#*
#*  Notes
#*
#*  History
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
package require snit
package require Img

snit::type IconImage {
    ## Icon image management
    # Auto loads images and keeps track of available and loaded image objects
    # for labels and related uses.
    
    pragma -hastypeinfo no
    pragma -hastypedestroy no
    pragma -hasinstances   no
    
    typevariable icondir
    ## @privatesection Directory where icons and images reside.
    typevariable unknownimg
    ## Image to use for unknown images.
    typevariable imagemap -array {}
    ## Map of available and loaded images.
    typeconstructor {
        ## Type constructor: one time initialization of type variables.
        set icondir [file join [file dirname \
                                [file dirname [file dirname \
                                               [info script]]]] Images]
        set unknownimg [image create photo -file [file join $icondir unknown.xpm]]
        snit::enum ftypes -values {any png xpm xbm}
    }
    typemethod insert {_self args} {
        ## Insert a new image.
        # @param _self Image name, also base file name.
        # @param ... Options:
        # @arg -filetype File type.
        # @arg -icondir Alternitive directory to look in.
        
        #puts stderr "*** $type insert $_self $args"
        set _name [namespace tail $_self]
        #puts stderr "*** $type insert $_self: _name is $_name"
        set filetype [from args -filetype "any"]
        set _icondir [from args -icondir $icondir]
        #puts stderr "*** $type insert $_self: filetype is $filetype"
        ftypes validate $filetype
        #puts stderr "*** $type insert $_self: filetype validated."
        switch $filetype {
            xbm {
                set xbmfile [file join $_icondir $_name.xbm]
                set xpmfile {}
                set pngfile {}
            }
            xpm {
                set xbmfile {}
                set xpmfile [file join $_icondir $_name.xpm]
                set pngfile {}
            }
            png {
                set xbmfile {}
                set xpmfile {}
                set pngfile [file join $_icondir $_name.png]
            }
            any {
                set xbmfile [file join $_icondir $_name.xbm]
                set xpmfile [file join $_icondir $_name.xpm]
                set pngfile [file join $_icondir $_name.png]
            }
        }
        #puts stderr "*** $type insert $_self: pngfile file is $pngfile"
        #puts stderr "*** $type insert $_self: xpmfile file is $xpmfile"
        #puts stderr "*** $type insert $_self: xbmfile file is $xbmfile"
        if {$pngfile ne {} && [file exists $pngfile]} {
            set imagemap($_name) [image create photo -file $pngfile]
        } elseif {$xpmfile ne {} && [file exists $xpmfile]} {
            set imagemap($_name) [image create photo -file $xpmfile]
        } elseif {$xbmfile ne {} && [file exists $xbmfile]} {
            set imagemap($_name) [image create bitmap -file $xbmfile \
                                 -background [from args -background {}] \
                                 -foreground [from args -foreground black]]
        } else {
            set imagemap($_name) $unknownimg
        }
        #puts stderr "*** $type insert $_self: imagemap($_name) is $imagemap($_name)"
        
    }
    typemethod image {name args} {
        ## @publicsection Fetch an image by name.
        # @param name Image name.
        # @param ... Options:
        # @arg -filetype File type.
        # @arg -icondir Alternitive directory to look in.
        
        #puts stderr "*** $type image $name $args"
        #parray imagemap
        set _name [namespace tail $name]
        #puts stderr "*** $type image: $_name"
        if {[::info exists imagemap($_name)]} {
            return $imagemap($_name)
        } else {
            eval $type insert $name {*}$args
            #puts stderr "*** $type image: imagemap($_name) is $imagemap($_name)"
            return $imagemap($_name)
        }
    }
}

snit::type IconBitmap {
    ## Icon bitmap management
    # Auto loads bitmaps and keeps track of available and loaded bitmap objects
    # for labels and related uses.
    
    pragma -hastypeinfo no
    pragma -hastypedestroy no
    pragma -hasinstances   no
    
    typevariable icondir
    ## @privatesection Directory where icons and images reside.
    typevariable unknownbm
    ## Bitmap to use for unknown bitmaps.
    typevariable bitmapmap -array {}
    ## Map of available and loaded bitmaps.
    
    typeconstructor {
        ## Type constructor: one time initialization of type variables.
        set icondir [file dirname [info script]]
        set unknownbm error
        foreach stockbm {error gray75 gray50 gray25 gray12 hourglass info questhead question warning} {
            set bitmapmap($stockbm) $stockbm
        }
        if {$::tcl_platform(platform) eq "macintosh"} {
            foreach macbm {document stationery edition application accessory folder pfolder trash floppy ramdisk cdrom preferences querydoc stop note caution} {
                set bitmapmap($macbm) $macbm
            }
        }
    }
    typemethod insert {_self args} {
        ## Insert a new bitmap.
        # @param _self Bitmap name, also base file name.
        # @param ... Options:
        # @arg -icondir Alternitive directory to look in.
        
        set name [namespace tail $_self]
        set _icondir [from args -icondir $icondir]
        set xbmfile [file join $_icondir $name.xbm]
        if {[file exists $xbmfile]} {
            set bitmapmap($name) @$xbmfile
        } else {
            set bitmapmap($name) $unknownbm
        }
    }
    typemethod bitmap {name} {
        ## @publicsection Fetch a bitmap by name.
        # @param name Bitmap name.
        
        if {[info exists bitmapmap($name)]} {
            return $bitmapmap($name)
        } else {
            $type insert $name
            return $bitmapmap($name)
        }
    }
}

## @}

package provide IconImage 1.0

