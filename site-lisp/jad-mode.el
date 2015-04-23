;; $Revision: 1.1.1.1 $

;; Copyright (C) 2000 by Ingo Koch

;; Author: ingo Koch <ingo.koch@ikoch.de>
;; The Idea and a lot of the code is stolen from the 
;; .emacs file of  Jake Donham <jake@bitmechanic.com>
;; available at http://www.jaked.org/emacs.html
;; who did this for the mocha java decompiler
;; Maintainer: Ingo Koch <ingo.koch@ikoch.de>
;; Keywords: java, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Description:

;; This package is an add-on to the Java Development Environment
;; (JDE) for Emacs. It automatically decompiles a class file and 
;; offers you a buffer to view or edit it.
;; javadecomp (currently) relies on the jad java decompiler to
;; do the actual work, but it should be possible to extend it to
;; whatever you like (sugestions are welcome).
;; jad is available at the Jad home page: 
;; http://www.geocities.com/SiliconValley/Bridge/8617/jad.html
;; by Pavel Kouznetsov (kpdus@yahoo.com).
;; It supports a wide range of OS like:
;; - Windows 95/NT on Intel platform 
;; - Linux on Intel platform 
;; - Solaris 7.0 on Intel platform
;; - Rhapsody 5.3 on PowerPC platform 
;; - AIX 4.2 on IBM RS/6000 platform 
;; - OS/2 
;; - Solaris 2.5 on SUN Sparc platform
;; - FreeBSD 2.2.x

;;; Installation:

;; Put the following in your .emacs file:
;;   (require 'javadecomp)
;;
;;; Usage:
;; Open a class file and feel happy.

;;; Support:

;; Any comments, suggestions, bug reports or upgrade requests are welcome.
;; Please send them to Ingo Koch at ingo.koch@ikoch.de.
;;

;;; Code:

(defvar jdc-command  "/usr/bin/jad"
  "The name of the decompiler if it's on your path, otherwise
a full qualified path to it.")

(defvar jdc-parameter  " -space -t4 -o "
  "Extra parameter which should be added to the call of the decompiler.")

(defvar jdc-extension  "jad"
  "The extension which is used for the generated java files.")

(defun jdc-buffer ()
  "Construct the command for decompiling a class file, call the resulting
command and load the decompiled file."
  
  (let*
      (
       (exists (file-exists-p (buffer-file-name)))
       (jdc-classfile (if exists
                          (file-name-nondirectory (buffer-file-name))
                        (let ((tempfile (concat "/tmp/" (file-name-nondirectory (buffer-file-name))))
                              (coding-system-for-write 'raw-text))
                          (write-region nil nil tempfile)
                          tempfile
                          )))
       ;; the regexp below replaces /tmp/jarname.jar:ClassName.class with ClassName.class
       (jdc-javafile (replace-regexp-in-string "[^/:]+:" "" (concat (substring jdc-classfile 0 -5) jdc-extension)))
       (command (concat jdc-command (when (not exists) " -d /tmp " ) jdc-parameter jdc-classfile " > /dev/null")))
    (shell-command command)
    (find-alternate-file jdc-javafile)))

(define-derived-mode jad-mode java-mode "jad-mode"
  (jdc-buffer)
)

(provide 'jad-mode)
