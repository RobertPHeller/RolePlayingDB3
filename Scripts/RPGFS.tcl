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
#  Last Modified : <220508.1534>
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
    proc fail {code} {
        ::vfs::filesystem posixerror $::vfs::posix($code)
    }
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
        method SetOffset {offset} {set _offset $offset}
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
        destructor {
            foreach f $_files {
                $f destroy
            }
        }
        method AddNewDirent {name args} {
            #puts stderr "*** ${self}::AddNewDirent $name $args"
            #puts stderr "*** ${self}::AddNewDirent: options(-ftype) is $options(-ftype)"
            if {$options(-ftype) ne "directory"} {::vfs::rpg::fail ENOENT}
            set newent [$type create %AUTO% $name -ftype [from args -ftype]]
            #puts stderr "*** ${self}::AddNewDirent: newent is $newent"
            lappend _files $newent
            #puts stderr "*** ${self}::AddNewDirent: _files is $_files"
            return $newent
        }
        method RemoveDirent {dirent} {
            if {$options(-ftype) ne "directory"} {::vfs::rpg::fail ENOENT}
            set i [lsearch -exact $_files $dirent]
            if {$i >= 0} {
                set _files [lreplace $_files $i $i]
            }
            return $dirent
        }
        method GetDirents {} {
            #puts stderr "*** ${self}::GetDirents: _files is $_files"
            if {$options(-ftype) ne "directory"} {::vfs::rpg::fail ENOENT}
            return $_files
        }
        method NumDirents {} {
            if {$options(-ftype) ne "directory"} {::vfs::rpg::fail ENOENT}
            return [llength $_files]
        }
        method LookupName {name} {
            if {$options(-ftype) ne "directory"} {::vfs::rpg::fail ENOENT}
            foreach f $_files {
                if {$name eq [$f Name]} {
                    return $f
                }
            }
            return {}
        }
        method LookupMatch {pattern {types {file directory}}} {
            set result [list]
            if {$options(-ftype) ne "directory"} {::vfs::rpg::fail ENOENT}
            foreach f $_files {
                if {[string match $pattern [$f Name]] &&
                    [$f cget -ftype] in $types} {
                    lappend result $f
                }
            }
            return $result
        }
        
    }
    
    snit::type RPGFileSystem {
        delegate method * to rootdirectory
        component rootdirectory
        variable _fd
        variable _backingfile
        variable _dirty no
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
            #puts stderr "*** ${self}::_reload_dirtree: treesize is $treesize"
            set tree [read $_fd $treesize]
            #puts stderr "*** ${self}::_reload_dirtree: tree is $tree"
            _copyDictToRoot $rootdirectory $tree
        }
        proc _copyDictToRoot {dir branch} {
            #puts stderr "*** _copyDictToRoot: $dir ([$dir Name]) $branch"
            foreach {name value} $branch {
                if {[lindex $value 0] ne {/FILE/}} {
                    set newdirent [$dir AddNewDirent $name -ftype directory]
                    _copyDictToRoot $newdirent $value
                } else {
                    set newdirent [$dir AddNewDirent $name -ftype file]
                    lassign $value dummy mtime mode off size
                    $newdirent SetMTime $mtime
                    $newdirent SetMode $mode
                    $newdirent SetOffset $off
                    $newdirent SetSize $size
                }
            }
        }
        destructor {
            if {$_dirty} {$self _flush_dirtree}
            close $_fd
            $rootdirectory destroy
        }
        proc _makedict {file} {
            #puts stderr "*** _makedict: file is $file, \[\$file Name] is [$file Name]"
            if {[$file cget -ftype] eq "file"} {
                return [list {/FILE/} [$file MTime] [$file Mode] \
                        [$file Offset] [$file Size]]
            } else {
                set tree [list]
                foreach d [$file GetDirents] {
                    lappend tree [$d Name] [_makedict $d]
                }
                return $tree
            }
        }
        method _flush_dirtree {} {
            set dirtree [_makedict $rootdirectory]
            #puts stderr "*** _flush_dirtree: dirtree is $dirtree"
            seek $_fd 0 end
            set treesize [string length $dirtree]
            puts -nonewline $_fd "$dirtree"
            puts -nonewline $_fd [binary format i $treesize]
        }
        method _handler {cmd root relative actualpath args} {
            #puts stderr "*** ${self}::_handler '$cmd' '$root' '$relative' '$actualpath' $args"
            switch -- $cmd {
                access {
                    return [$self _access $relative [lindex $args 0]]
                }
                createdirectory {
                    return [$self _createdirectory $relative]
                }
                deletefile {
                    return [$self _deletefile $relative]
                }
                fileattributes {
                    return [$self _fileattributes $relative {*}$args]
                }
                matchindirectory {
                    return [$self _matchindirectory $relative {*}$args]
                }
                open {
                    return [$self _open $relative {*}$args]
                }
                removedirectory {
                    return [$self _removedirectory $relative {*}$args]
                }
                stat {
                    return [$self _stat $relative]
                }
                utime {
                    return [$self _utime $relative {*}$args]
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
        method _utime {path atime mtime} {
            #puts stderr "*** ${self}::_utime $path $atime $mtime"
            set pathkeys [file split $path]
            set dirent [_findDirent $rootdirectory $pathkeys]
            if {$dirent eq ""} {::vfs::rpg::fail  ENOENT}
            $dirent SetMTime $mtime
            set _dirty yes
        }
        method _access {filename mode} {
            #puts stderr "*** ${self}::_access $filename $mode"
            set pathkeys [file split $filename]
            #puts stderr "*** ${self}::_access: \{$pathkeys\}"
            set dirent [_findDirent $rootdirectory $pathkeys]
            if {$dirent eq ""} {::vfs::rpg::fail  ENOENT}
            if {$mode == 0} {return}
            set fmode [$dirent Mode]
            if {($mode & $fmode) == 0} {::vfs::rpg::fail EACCES}
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
            set _dirty yes
        }
        method _stat {path} {
            #puts stderr "*** ${self}::_stat $path"
            set pathkeys [file split $path]
            #puts stderr "*** ${self}::_stat: \{$pathkeys\}"
            set dirent [_findDirent $rootdirectory $pathkeys]
            if {$dirent eq {}} {::vfs::rpg::fail ENOENT}
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
        method _deletefile {path} {
            puts stderr "*** ${self}::_deletefile $path"
            if {$path eq "."  || $path eq ""} {::vfs::rpg::fail EACCES}
            set pathkeys [file split $path]
            set dirpathkeys [lrange $pathkeys 0 end-1]
            set filename [lindex $pathkeys end]
            set dirent [_findDirent $rootdirectory $dirpathkeys]
            if {$dirent eq {}} {::vfs::rpg::fail ENOENT}
            set filedirent [$dirent LookupName $filename]
            if {$filedirent eq {}} {return}
            if {[$filedirent cget -type] eq "directory"} {
                ::vfs::rpg::fail EISDIR
            }
            set deleteddirent [$dirent RemoveDirent $filedirent]
            $deleteddirent destroy
            set _dirty yes
        }
        method _removedirectory {path recursiveP} {
            puts stderr "*** ${self}::_removedirectory $path $recursiveP"
            if {$path eq "."  || $path eq ""} {::vfs::rpg::fail EACCES}
            set pathkeys [file split $path]
            set dirpathkeys [lrange $pathkeys 0 end-1]
            set filename [lindex $pathkeys end]
            set dirent [_findDirent $rootdirectory $dirpathkeys]
            if {$dirent eq {}} {::vfs::rpg::fail ENOENT}
            set filedirent [$dirent LookupName $filename]
            if {$filedirent eq {}} {return}
            if {![$filedirent cget -type] eq "directory"} {
                ::vfs::rpg::fail ENOTDIR
            }
            if {!$recursiveP && [$filedirent NumDirents] > 0} {
                ::vfs::rpg::fail EISDIR
            }
            set deleteddirent [$dirent RemoveDirent $filedirent]
            $deleteddirent destroy
            set _dirty yes
        }
        method _fileattributes {$path args} {
            puts stderr "*** ${self}::_fileattributes $path $args"
            set pathkeys [file split $path]
            set dirent [_findDirent $rootdirectory $pathkeys]
            if {$dirent eq {}} {::vfs::rpg::fail ENOENT}
            if {[llength $args] == 0} {
                return [list -permissions [format {0%o} [$dirent Mode]]]
            } elseif {[llength $args] == 1} {
                if {[lindex $args 0] ne "-permissions"} {::vfs::rpg::fail ENODEV}
                return [format {0%o} [$dirent Mode]]
            } else {
                ::vfs::rpg::fail ENODEV
            }
        }
        method _matchindirectory {path pattern types args} {
            if {$pattern eq {}} {
                if {![::vfs::matchDirectories $types]} {
                    return {}
                } else {
                    return $path
                }
            }
            set returntypes [list]
            if {[::vfs::matchDirectories $types]} {
                lappend returntypes directory
            }
            if {[::vfs::matchFiles $types]} {
                lappend returntypes file
            }
            set pathkeys [file split $path]
            set dirent [_findDirent $rootdirectory $pathkeys]
            set result [list]
            foreach f [$dirent LookupMatch $pattern $returntypes] {
                lappend result [file join [namespace tail $self] $path [$f Name]]
            }
            return $result
        }
        method _open {path mode permissions} {
            if {$mode eq {}} {set mode r}
            # workaround: Tclvfs can't handle channels in write-only modes; see Tclvfs bug #1004273
            if {$mode eq "w"} {set mode w+}
            if {$mode eq "a"} {set mode a+}
            set pathkeys [file split $path]
            set dirpath [lrange $pathkeys 0 end-1]
            set filename [lindex $pathkeys end]
            set directorydirent [_findDirent $rootdirectory $dirpath]
            set dirent [$directorydirent LookupName $filename]
            if {$directorydirent eq {}} {::vfs::rpg::fail ENOENT}
            # if mode is read or read+write, the file must already exist
            if {$mode in {r r+} && $dirent eq {}} {::vfs::rpg::fail ENOENT}
            set channelID [vfs::memchan $filename]
            set defaulttrans [fconfigure $channelID -translation]
            if {$mode ni {w w+} && $dirent ne {}} {
                # Append to an existing file
                seek $_fd [$dirent Offset] start
                fconfigure $channelID -translation binary
                puts -nonewline $channelID [read $_fd [$dirent Size]]
                fconfigure $channelID -translation $defaulttrans
                if {$mode in {r r+}} {
                    seek $channelID 0 start
                }
            }
            if {$dirent eq {}} {
                set dirent [$directorydirent AddNewDirent $filename -ftype file]
            }
            return [list $channelID [mymethod _close $channelID $dirent $mode]]
        }
        method _close {channelID dirent mode} {
            if {$mode in {r r+}} {return}
            seek $_fd 0 end
            seek $channelID 0 end
            set filesize [tell $channelID]
            set offset [tell $_fd]
            seek $channelID 0 start
            fconfigure $channelID -translation binary
            puts -nonewline $_fd [read $channelID $filesize]
            $dirent SetOffset $offset
            $dirent SetSize   $filesize
            $dirent SetMTime  [clock seconds]
            set _dirty yes
        }
    }
    
    proc Mount {mkfile local args} {
        if {[info exists $local] && [RPGFileSystem validate $local]} {
            error "Duplicate mount: $mkfile $local"
        }
        return [eval [list RPGFileSystem create $local $mkfile] $args]
    }
        
}

package provide vfs::rpg 1.0
