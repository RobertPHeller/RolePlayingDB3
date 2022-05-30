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
#  Last Modified : <220530.1046>
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

## @addtogroup RPGSupport
# @{

package require vfs
package require snit


namespace eval vfs::rpg {
    ## Roleplaying Database temp filesystem.
    # This is an implementation of a simple Virtual Filesystem for internal use
    # by the Roleplaying Database program.  It stores the working version of
    # the RPG DB bundle which is normally stored as a ZIP file.  It is a basic
    # and simple file system, using a backing file as temporary file system 
    # storage.
    #
    # 
    
    proc fail {code} {
        ## Generate a posixerror
        # @param code -- the posix error to generate.
        
        ::vfs::filesystem posixerror $::vfs::posix($code)
    }
    snit::enum FileType -values {file directory}
    ## @enum FileType -- flags the type of directory entry.
    snit::type RPGDirent {
        ## @brief A Directory Entry
        # Holds information about a file or directory.  Only the bare minimum
        # information is kept: name, mod time, list of files (directories only),
        # mode (permissions), offset (files only), and size (files only).
        #
        # Options:
        # @arg -ftype  This readonly option flags type of directory entry,
        #              file or directory.
        # @par
        # Instance variables:
        # @arg _name   The file name.  Accessors: Name and SetName.
        # @arg _mtime  The mod time.   Accessors: MTime and SetMTime.
        # @arg _files  The list of files (directories only).  No direct 
        #              accessors.
        # @arg _mode   The file mode (permissions). Accessors Mode and SetMode.
        # @arg _offset The offset in the backing file (files only). Accessors
        #              Offset and SetOffset.
        # @arg _size   The size of the file (files only). Accessors Size and
        #              SetSize
        
        variable _name
        method Name {} {return $_name}
        method SetName {name} {set _name $name}
        option -ftype -readonly yes -default file -type ::vfs::rpg::FileType
        variable _mtime 0
        method MTime {} {return $_mtime}
        method SetMTime {time} {set _mtime $time}
        variable _files [list]
        variable _mode 0100744
        method Mode {} {return $_mode}
        method SetMode {mode} {set _mode $mode}
        variable _offset 0
        method Offset {} {return $_offset}
        method SetOffset {offset} {set _offset $offset}
        variable _size 0
        method Size {} {return $_size}
        method SetSize {size} {set _size $size}
        constructor {name args} {
            ## Constructor: create a new direct.  Not normally called directly
            # (see @ref AddNewDirent).
            #
            # @param objname Should be passed as %AUTO%
            # @param name New filename
            # @param ... Options:
            # @arg -ftype File type.  Must be either "file" or "directory". 
            #             Readonly and defaults to "file".
            # @par
            
            #puts stderr "*** ${type} create $self $name $args"
            #puts stderr "*** ${type} create: self is '$self'"
            #puts stderr "*** ${type} create: args are '$args'"
            set _name $name
            $self configurelist $args
            if {$options(-ftype) eq "directory"} {
                set _mode 0040744
            }
        }
        destructor {
            ## Destructor -- destroy the directory entry and all of its child
            # directory entries.
            foreach f $_files {
                $f destroy
            }
        }
        method AddNewDirent {name args} {
            ## Add a new directory entry to this directory.
            # @param name The new filename.
            # @param ... Options:
            # @arg -ftype The type of new directory entry to create.  Must be
            # one of "file" or "directory" and is "file" by default.
            # @par
            # @returns The new directory entry.
            # @throws ENOENT If the containing dirent is not a directory.
            
            #puts stderr "*** ${self}::AddNewDirent $name $args"
            #puts stderr "*** ${self}::AddNewDirent: options(-ftype) is $options(-ftype)"
            if {$options(-ftype) ne "directory"} {::vfs::rpg::fail ENOENT}
            set newent [$type create %AUTO% $name -ftype [from args -ftype file]]
            #puts stderr "*** ${self}::AddNewDirent: newent is $newent"
            lappend _files $newent
            #puts stderr "*** ${self}::AddNewDirent: _files is $_files"
            return $newent
        }
        method RemoveDirent {dirent} {
            ## Remove a directory entry from this directory entry.
            # The dirent is not destroyed!  The calling function needs to 
            # take care of that or suffer a memory leak.
            #
            # @param dirent The dirent to remove.
            # @returns The dirent.
            # @throws ENOENT If the containing dirent is not a directory.
            # (No error is reported if the dirent being removed is not
            # in this dirent.)

            if {$options(-ftype) ne "directory"} {::vfs::rpg::fail ENOENT}
            set i [lsearch -exact $_files $dirent]
            if {$i >= 0} {
                set _files [lreplace $_files $i $i]
            }
            return $dirent
        }
        method GetDirents {} {
            ## Get a list of child dirents.
            # @returns A list of dirents.
            # @throws ENOENT If the containing dirent is not a directory.
            
            #puts stderr "*** ${self}::GetDirents: _files is $_files"
            if {$options(-ftype) ne "directory"} {::vfs::rpg::fail ENOENT}
            return $_files
        }
        method NumDirents {} {
            ## Get a count of child dirents.
            # @returns The number of child dirents.
            # @throws ENOENT If the containing dirent is not a directory.
            
            if {$options(-ftype) ne "directory"} {::vfs::rpg::fail ENOENT}
            return [llength $_files]
        }
        method LookupName {name} {
            ## Lookup a filename (exact match).
            # @param name The filename to look for.
            # @returns A dirent or {}.
            # @throws ENOENT If the containing dirent is not a directory.
            
            if {$options(-ftype) ne "directory"} {::vfs::rpg::fail ENOENT}
            foreach f $_files {
                if {$name eq [$f Name]} {
                    return $f
                }
            }
            return {}
        }
        method LookupMatch {pattern {types {file directory}}} {
            ## LookupMatch using a glob pattern
            # @param pattern The (glob) pattern to look for.
            # @param types The file types to check for.  Must be a list 
            # containing "file" and/or "directory"
            # @returns A list of dirents.
            # @throws ENOENT If the containing dirent is not a directory.
            
            #puts stderr "*** $self LookupMatch $pattern $types"
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
        ## @brief The filesystem class.
        # This is the class that implements a simple Virtual Filesystem for 
        # internal use by the Roleplaying Database program. It stores the
        # working version of the RPG DB bundle which is normally stored as a 
        # ZIP file. It is a basic and simple file system, using a backing file 
        # as temporary file system storage.
        
        delegate method * to rootdirectory
        component rootdirectory
        ## @privatesection Root directory of the file system.
        variable _fd
        variable _backingfile
        variable _dirty no
        constructor {backingfile args} {
            ## @publicsection Constructor -- takes care of mounting the file
            # system.  The root directory is set up.  If the backing file
            # already exists, the root directory is read in from the backing 
            # file.
            # @param mountpoint The name of the mount point.
            # @param backingfile The name of the backing file.  It is created
            # if it does not exist.
            
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
            ## @privatesection Unmount the file system.  The mount point is destroyed (see the
            # destructor for details).
            # @param mp (not used).
            
            $self destroy
        }
        method _reload_dirtree {} {
            ## Reload the directory tree from the backing file.
            
            seek $_fd -4 end
            binary scan [read $_fd 4] i treesize
            seek $_fd [expr {-(4+$treesize)}] end
            #puts stderr "*** ${self}::_reload_dirtree: treesize is $treesize"
            set tree [read $_fd $treesize]
            #puts stderr "*** ${self}::_reload_dirtree: tree is $tree"
            _copyDictToRoot $rootdirectory $tree
        }
        proc _copyDictToRoot {dir branch} {
            ## Copy the dictionary tree from the file to the dirent tree.
            # @param dir Directory dirent
            # @param branch Current dictionary tree, in the form 
            # {name value name value ...}.  Name is a filename, value is either
            # a file's properties or a another branch if filename is a 
            # directory.
            
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
            ## @publicsection Destructor -- unmount the filesystem and free up
            # all allocated memory.  Flush the directory tree to the backing 
            # file if it is "dirty".
            
            if {$_dirty} {$self _flush_dirtree}
            close $_fd
            $rootdirectory destroy
        }
        proc _makedict {file} {
            ## @privatesection Convert a dirent to a dictionary in order to
            # store it in the backing file.
            # @param file A dirent object
            # @returns A dictionary branch.
            
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
            ## Flush the directory tree to the backing file.
            
            set dirtree [_makedict $rootdirectory]
            #puts stderr "*** _flush_dirtree: dirtree is $dirtree"
            seek $_fd 0 end
            set treesize [string length $dirtree]
            puts -nonewline $_fd "$dirtree"
            puts -nonewline $_fd [binary format i $treesize]
        }
        method _handler {cmd root relative actualpath args} {
            ## File system handler.  Called from the VFS system C code.
            # @param cmd The command to handle
            # @param root 
            # @param relative The filename
            # @param actualpath
            # @param ... Additional args (depends on the command).
            
            if {$relative eq "."} {set relative {}}
            set relative [regsub {^\./} $relative {}]
            set relative [regsub {/\.$} $relative {}]
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
            ## Find a directory entry
            # @param parent Parent directory entry
            # @param pathlist List of pathname elements (result of file split).
            # @returns A directory entry or {}
            
            #puts stderr "*** _findDirent $parent $pathlist"
            if {$parent eq {}} {
                return {}
            } elseif {[llength $pathlist] == 0} {
                return $parent
            } else {
                return [_findDirent [$parent LookupName [lindex $pathlist 0]] \
                        [lrange $pathlist 1 end]]
            }
        }
        method _utime {path atime mtime} {
            ## Update the file's mod time.
            # @param path The file to update.
            # @param atime (not used)
            # @param mtime The new mod time.
            # @throws ENOENT If the file does not exist.
            
            #puts stderr "*** ${self}::_utime $path $atime $mtime"
            set pathkeys [file split $path]
            set dirent [_findDirent $rootdirectory $pathkeys]
            if {$dirent eq ""} {::vfs::rpg::fail  ENOENT}
            $dirent SetMTime $mtime
            set _dirty yes
        }
        method _access {filename mode} {
            ## Implements file access checks
            # @param filename The name of the file to check.
            # @param mode     The modes to check for.  0 means existance, 
            #                 otherwise this is a bit map of permissions 
            #                 (Read, Write, Execute).
            # @throws  ENOENT if the file does not exist, EACCES if the access
            # mode is not allowed.
            
            #puts stderr "*** ${self}::_access $filename $mode"
            set pathkeys [file split $filename]
            #puts stderr "*** ${self}::_access: \{$pathkeys\}"
            set dirent [_findDirent $rootdirectory $pathkeys]
            if {$dirent eq ""} {::vfs::rpg::fail  ENOENT}
            if {$mode == 0} {return}
            set fmode [$dirent Mode]
            #puts stderr [format {*** %s::_access: mode = 0%o, fmode = 0%o} $self $mode $fmode]
            if {($mode & ($fmode >> 6)) == 0} {::vfs::rpg::fail EACCES}
        }
        method _createdirectory {newdire} {
            ## Implements mkdir.
            # @param newdire The directory to be created.
            # @throws ENOENT If the parent directory does not exist.
            
            #puts stderr "*** ${self}::_createdirectory $newdire"
            set parentpathkeys [file split [file dirname $newdire]]
            if {[lindex $parentpathkeys 0] eq "."} {
                set parentpathkeys [lrange $parentpathkeys 1 end]
            }
            set newdirname [file tail $newdire]
            #puts stderr "*** ${self}::_createdirectory: parentpathkeys is \{$parentpathkeys\}"
            #puts stderr "*** ${self}::_createdirectory: newdirname is $newdirname"
            set parent [_findDirent $rootdirectory $parentpathkeys]
            if {$parent eq {}} {::vfs::rpg::fail  ENOENT}
            $parent AddNewDirent $newdirname -ftype directory
            set _dirty yes
        }
        method _stat {path} {
            ## Implements stat
            # @param path The filename to get information about.
            # @returns Flat key value list containing the file's stat.
            # @throws ENOENT if the file does not exist.
            
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
            ## Implements file delete
            # @param path The filename to delete.
            # @throws EACCES when attempting to delete the root,
            #         ENOENT if the parent directory does not exist,
            #         EISDIR if the path is a directory.
            #         Quietly does nothing if the file itself does not exist.
            
            #puts stderr "*** ${self}::_deletefile $path"
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
            ## Implements rmdir
            # @param path The directory to remove.
            # @param recursiveP Boolean flag to select recursive removal.
            # @throws EACCES when attempting to delete the root,
            #         ENOENT if the parent directory does not exist
            #         ENOTDIR if the path is not a directory
            #         EISDIR is recursiveP is false and the directory is not
            #         empty.
            #         Quietly does nothing if the directory itself does not 
            #         exist.
            
            #puts stderr "*** ${self}::_removedirectory $path $recursiveP"
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
        method _fileattributes {path args} {
            ## Implements getting or setting a file's attributes.
            # Currently the only supported attribute is -permissions.
            # @param path The file to get attributes about
            # @param ... Options:
            # @args -permissions get/set a files permissions (mode)
            # @par
            # @returns All of the attributes if no options specificed or
            # the specified attribute's value.
            # @throws ENOENT if the file does not exist
            #         ENODEV if an attribute other than -permissions is 
            #                specified or if it is attempted to change the 
            #                file's permissions.
            
            #puts stderr "*** ${self}::_fileattributes $path $args"
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
            ## Implements file globbing
            # @param path Directory to look in.
            # @param pattern Glob pattern to match against
            # @param types Types of files to look for
            # @param ... Options: (none)
            # @returns list of full pathnames that match.
            
            #puts stderr "*** $self _matchindirectory $path $pattern $types $args"
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
            #puts stderr "*** $self _matchindirectory: pathkeys is $pathkeys"
            set dirent [_findDirent $rootdirectory $pathkeys]
            #puts stderr "*** $self _matchindirectory: dirent is $dirent"
            if {$dirent eq {}} {
                ::vfs::rpg::fail ENOENT
            }
            set result [list]
            foreach f [$dirent LookupMatch $pattern $returntypes] {
                #puts stderr "*** $self _matchindirectory (result loop): f is '[$f Name]'"
                lappend result [file join [namespace tail $self] $path [$f Name]]
            }
            #puts stderr [list *** $self _matchindirectory: result is $result]
            return $result
        }
        method _open {path mode permissions} {
            ## Opens a file
            # @param path Path of file to open
            # @param mode Mode to open file: r, r+, w, w+, a, a+. Defaults to r.
            # @param permissions Permissions (not used).
            # @returns A list containing the file handle and the close hook.
            # @throws ENOENT if the parent directory does not exist, or if
            # the file does not exist when the mode is r or r+.
            
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
            ## Close a file
            # Files opened for read (mode is r or r+): nothing is done.
            # Otherwise, the file is copied to the backing file.
            # @param channelID File channel.
            # @param dirent The file's dirent.
            # @param mode The file's open mode.
            
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
        ## Public function to mount a RPGFileSystem file system
        # @param mkfile Backing file
        # @param local Mount point
        # @param ... Options: (none)
        # @returns A RPGFileSystem object, bound to the mountpoint.
        
        if {[info exists $local] && [RPGFileSystem validate $local]} {
            error "Duplicate mount: $mkfile $local"
        }
        return [RPGFileSystem create $local $mkfile {*}$args]
    }
        
}

## @}

package provide vfs::rpg 1.0
