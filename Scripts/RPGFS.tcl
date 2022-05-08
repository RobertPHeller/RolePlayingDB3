#*****************************************************************************
#
#  System        : 
#  Module        : 
#  Object Name   : $RCSfile$
#  Revision      : $Revision$
#  Date          : $Date$
#  Author        : $Author$
#  Created By    : Robert Heller
#  Created       : Sat May 7 13:39:32 2022
#  Last Modified : <220507.2048>
#
#  Description	
#
#  Notes
#
#  History
#	
#*****************************************************************************
#
#    Copyright (C) 2022  Robert Heller D/B/A Deepwoods Software
#			51 Locke Hill Road
#			Wendell, MA 01379-9728
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
# 
#
#*****************************************************************************

package require vfs
package require snit


namespace eval ::vfs::rpg {
    snit::enum FileType -values {file directory}
    snit::type RPGDirent {
        variable _name
        method Name {} {return $_name}
        method SetName {name} {set _name $name}
        option -ftype -readonly yes -default file -type ::vfs::rpg::FileType
        variable _mtime 0
        method MTime {} {return $_mtime}
        method SetMTime {time} {set _mtime $time}
        variable _files [list]
        variable _mode 0777
        method Mode {} {return $_mode}
        method SetMode {mode} {set _mode $mode}
        variable _offset 0
        method Offset {} {return $_offset}
        method SetOffset {offset} {set _offset $_offset}
        variable _size 0
        method Size {} {return $_size}
        method SetSize {size} {set _size $size}
        constructor {name args} {
            #puts stderr "*** ${type} create $self $name $args"
            #puts stderr "*** ${type} create: self is '$self'"
            #puts stderr "*** ${type} create: args are '$args'"
            set _name $name
            $self configurelist $args
        }
        method AddNewDirent {name args} {
            #puts stderr "*** ${self}::AddNewDirent $name $args"
            #puts stderr "*** ${self}::AddNewDirent: options(-ftype) is $options(-ftype)"
            if {$options(-ftype) ne "directory"} {fail ENOENT}
            set newent [$type create %AUTO% $name -ftype [from args -ftype]]
            #puts stderr "*** ${self}::AddNewDirent: newent is $newent"
            lappend _files $newent
            return $newent
        }
        method RemoveDirent {dirent} {
            if {$options(-ftype) ne "directory"} {fail ENOENT}
            set i [lsearch -exact $_files $dirent]
            if {$i >= 0} {
                set _files [lreplace $_files $i $i]
            }
            return $dirent
        }
        method GetDirents {} {
            if {$options(-ftype) ne "directory"} {fail ENOENT}
            return $_files
        }
        method LookupName {name} {
            if {$options(-ftype) ne "directory"} {fail ENOENT}
            foreach f $_files {
                if {$name eq [$f Name]} {
                    return $f
                }
            }
            return {}
        }
        
    }
    
    snit::type RPGFileSystem {
        delegate method * to rootdirectory
        component rootdirectory
        variable _fd
        variable _backingfile
        variable _freespace 0
        constructor {backingfile args} {
            install rootdirectory using RPGDirent %AUTO% . -ftype directory
            set _backingfile [::file normalize $backingfile]
            if {[file exists $_backingfile]} {
                set _fd [open $_backingfile r+]
                fconfigure $_fd -translation binary
                $self _reload_dirtree
            } else {
                set _fd [open $_backingfile w+]
                fconfigure $_fd -translation binary
            }
            
            set mp [namespace tail $self]
            vfs::filesystem mount $mp [mymethod _handler]
            vfs::RegisterMount $mp [mymethod _unmount]
            return $self
        }
        method _unmount {mp} {
            $self destroy
        }
        method _reload_dirtree {} {
            seek $_fd -4 end
            binary scan [read $_fd 4] i treesize
            seek $_fd [expr {-(4+$treesize)}] end
            puts stderr "*** *** ${self}::_reload_dirtree: treesize is $treesize"
            set tree [read $_fd $treesize]
            puts stderr "*** ${self}::_reload_dirtree: tree is $tree"
            _copyDictToRoot $rootdirectory $tree
        }
        proc _copyDictToRoot {dir branch} {
            puts stderr "*** _copyDictToRoot: $dir ([$dir Name]) $branch"
            foreach k [dict keys $branch] {
                set value [dict get $branch $k]
                if {[lindex $value 0] ne {/FILE/}} {
                    set newdirent [$dir AddNewDirent $k -ftype directory]
                    _copyDictToRoot $newdirent $value
                } else {
                    set newdirent [$branch AddNewDirent $k -ftype file]
                    lassign $value dummy mtime mode off size
                    $newdirent SetMTime $mtime
                    $newdirent SetMode $mode
                    $newdirent SetOffset $off
                    $newdirent SetSize $size
                }
            }
        }
        destructor {
            $self _flush_dirtree
            close $_fd
        }
        proc _makedict {file} {
            puts stderr "*** _makedict: file is $file, \[\$file Name] is [$file Name]"
            if {[$file cget -ftype] eq "directory"} {
                set tree {}
                foreach d [$file GetDirents] {
                    dict set tree [$d Name] [_makedict $d]
                }
                return $tree
            } else {
                return [list {/FILE/} [$file MTime] [$file Mode] \
                        [$file Offset] [$file Size]]
            }
        }
        method _flush_dirtree {} {
            set dirtree [_makedict $rootdirectory]
            puts stderr "*** _flush_dirtree: dirtree is $dirtree"
            seek $_fd $_freespace start
            set treesize [string length $dirtree]
            puts -nonewline $_fd "$dirtree"
            puts -nonewline $_fd [binary format i $treesize]
        }
        method _handler {cmd root relative actualpath args} {
            puts stderr "*** ${self}::_handler '$cmd' '$root' '$relative' '$actualpath' $args"
            switch -- $cmd {
                access {
                    return [$self _access $relative [lindex $args 0]]
                }
                createdirectory {
                    return [$self _createdirectory $relative]
                }
                stat {
                    return [$self _stat $relative]
                }
            }
        }
        proc _findDirent {parent pathlist} {
            #puts stderr "*** _findDirent $parent $pathlist"
            if {[llength $pathlist] == 0} {
                return $parent
            } else {
                return [_findDirent [$parent LookupName [lindex $pathlist 0]] \
                        [lrange $pathlist 1 end]]
            }
        }
            
        method _access {filename mode} {
            #puts stderr "*** ${self}::_access $filename $mode"
            set pathkeys [file split $filename]
            #puts stderr "*** ${self}::_access: \{$pathkeys\}"
            set dirent [_findDirent $rootdirectory $pathkeys]
            if {$dirent eq ""} {fail  ENOENT}
            if {$mode == 0} {return}
            set fmode [$dirent Mode]
            if {($mode & $fmode) == 0} {fail  ENOENT}
        }
        method _createdirectory {newdire} {
            #puts stderr "*** ${self}::_createdirectory $newdire"
            set parentpathkeys [file split [file dirname $newdire]]
            if {[lindex $parentpathkeys 0] eq "."} {
                set parentpathkeys [lrange $parentpathkeys 1 end]
            }
            set newdirname [file tail $newdire]
            #puts stderr "*** ${self}::_createdirectory: parentpathkeys is \{$parentpathkeys\}"
            #puts stderr "*** ${self}::_createdirectory: newdirname is $newdirname"
            set parent [_findDirent $rootdirectory $parentpathkeys]
            $parent AddNewDirent $newdirname -ftype directory
        }
        method _stat {path} {
            #puts stderr "*** ${self}::_stat $path"
            set pathkeys [file split $path]
            #puts stderr "*** ${self}::_stat: \{$pathkeys\}"
            set dirent [_findDirent $rootdirectory $pathkeys]
            if {$dirent eq {}} {fail ENOENT}
            set sb(type) [$dirent cget -ftype]
            set sb(mtime) [$dirent MTime]
            set sb(mode) [$dirent Mode]
            set sb(size) [$dirent Size]
            set sb(dev) -1
            set sb(uid) -1
            set sb(gid) -1
            set sb(nlink) 1
            set sb(blksize) 0
            set sb(blocks) 0
            set sb(atime) $sb(mtime)
            set sb(ctime) $sb(mtime)
            return [array get sb]
        }
    }
    
    proc Mount {mkfile local args} {
        if {[info exists $local] && [RPGFileSystem validate $local]} {
            error "Duplicate mount: $mkfile $local"
        }
        return [eval [list RPGFileSystem create $local $mkfile] $args]
    }
        
}
