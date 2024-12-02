;;; advent-of-code-helper.el --- Emacs Interface for Advent of Code               -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Brandon C. Irizarry

;; Author: Brandon C. Irizarry <brandon.irizarry@gmail.com>
;; Keywords: advent-of-code, game, games, holidays

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; To install, put this file somewhere in your path and require this
;; file.
;;
;; Advent of Code is an Advent calendar of small programming puzzles
;; for a variety of skill sets and skill levels that can be solved in
;; any programming language you like. People use them as interview
;; prep, company training, university coursework, practice problems, a
;; speed contest, or to challenge each other.
;;
;; See 'https://adventofcode.com/2023/about'.
;;
;; In order to play Advent of Code, you need to set up an account with
;; it and log in. You then solve the available puzzles for a given
;; year. In order to do so, you first need to download that puzzle's
;; input, which is what you'll work with to solve the puzzle.
;;
;; Normally, you would download this input manually, specifying where
;; it should be downloaded onto disk. However, it's possible to
;; programmatically pull it down to local storage, as long as your
;; login cookie hash, inspectable via your browser's development
;; tools, is loaded into Emacs. See `url-cookie-list'.
;;
;; In addition, it's possible to programmatically make a POST request
;; to Advent of Code, when you want to submit a solution.
;;
;; This library provides a few utilities for facilitating these
;; concerns: one for setting the cookie's hash, a process we call
;; "bootstrapping"; one for then downloading the puzzle input into a
;; particular directory, which is created automatically if missing;
;; one for visiting a particular puzzle directory, obviating the need
;; to remember where you have your Advent of Code work stored; and one
;; for recording and possibly submitting a solution.
;;
;; We also define a minor mode, which uses a keymap that in turn lets
;; us define a simple menu enabling these tasks. It's of course
;; possible to add bindings to the keymap directly; we avoid doing
;; this out of the box to stay as much out of the user's way as
;; possible.

;;; Customization and defvars.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup advent-of-code-helper nil
  "A library for organizing Advent of Code solutions.

In particular, this library manages a user's cookie hash for
automating both downloading puzzle input, and uploading
solutions."
  :version 1.0
  :group 'local)

(defcustom aoch-top-level-directory "~/adventofcode/"
  "The top level directory containing your Advent of Code solutions."
  :type 'directory
  :tag "Advent of Code Helper Top Level Directory"
  :group 'advent-of-code-helper)

(defcustom aoch-cookie-name "cookie.eieio"
  "The name of the file containing your Advent of Code cookie hash.

This file is stored directly under your top-level directory."
  :type 'string
  :set-after '(aoch-top-level-directory)
  :tag "Advent of Code Helper Cookie Hash Filename"
  :group 'advent-of-code-helper)

(define-inline aoch-get-cookie-fullpath ()
  "Get the full path to the cookie, based on the current values of
`aoch-top-level-directory' and `aoch-cookie-name'."
  (concat aoch-top-level-directory
          aoch-cookie-name))

;;; EIEIO definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'eieio-custom)
(require 'eieio-base)

(defclass aoch-cookie (eieio-persistent eieio-singleton eieio-named)
  ((hash :initarg :hash
         :initform ""
         :reader get-hash
         :custom string
         :label "Cookie hash"
         :documentation
         "Used to validate Advent of Code HTTP requests."))
  :documentation "Advent of Code validation cookie.")

(cl-defmethod initialize-instance :after ((cookie aoch-cookie) &rest _)
  "Initialize fields inherited from EIEIO-PERSISTENT to their proper
values."
  (let ((fullpath (aoch-get-cookie-fullpath)))
    (unless (file-exists-p fullpath)
      (make-empty-file fullpath))
    (oset cookie file fullpath)
    (oset cookie file-header-line ";; -*- mode: lisp-data -*-")
    (oset cookie do-backups nil)
    (oset cookie object-name "Advent of Code Session Cookie")))

;; This function is a pared-down version of `eieio-persistent-save'.
;; It's meant to work with both the login cookie and submission
;; objects.
(cl-defmethod aoch-save ((obj eieio-persistent))
  "Save a persistent object OBJ to disk, using its :file field.

The comment header is a file-local-variable property line
specifying the major mode as `lisp-data-mode'."
  (with-temp-buffer
    (let ((standard-output (current-buffer))
          (eieio-print-object-name nil))
      (object-write obj)
      (let ((backup-inhibited (not (oref obj do-backups)))
            (coding-system-for-write 'utf-8-emacs))
        (write-region (point-min) (point-max) (oref obj file))))))

(cl-defmethod eieio-done-customizing ((cookie aoch-cookie))
  "Override this hook to ensure that the cookie is saved to disk
after customization."
  (aoch-save cookie))

(defun aoch-load-cookie ()
  "Load a cookie from `aoch-cookie-fullpath'."
  (eieio-persistent-read (aoch-get-cookie-fullpath) aoch-cookie))

(defun aoch-bootstrap ()
  "Bootstrap cookie by defining it via an EIEIO customization
buffer.

`eieio-done-customizing' is overriden to ensure that the new cookie
is saved to disk after customization."
  (interactive)
  (eieio-customize-object (aoch-cookie)))

;;; Main application.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun aoch--get-year-and-day-from-user ()
  "Helper function to use in `interactive' forms that require YEAR
and DAY as user-supplied parameters."
  (let ((current-year (string-to-number (format-time-string "%Y"))))
    (mapcar #'string-to-number
            (list
             (completing-read "Year: " (mapcar #'number-to-string (number-sequence 2015 current-year)))
             (completing-read "Day: " (mapcar #'number-to-string (number-sequence 1 25)))))))

(define-inline aoch--get-puzzle-directory (year day)
  "Return the string denoting the puzzle directory for a given YEAR
and DAY."
  (inline-quote (format "%s%d/day/%d" aoch-top-level-directory ,year ,day)))

;; This is more useful for debugging, when I've recently started Emacs
;; and want to mess around with `url-retrieve' and similar, but don't
;; have the cookie actually loaded yet.
(defun aoch--load-and-store-cookie ()
  "Helper function to load the cookie object from disk, and then
load the actual hash into Emacs with `url-cookie-store'."
  (let ((cookie (condition-case nil
                    (aoch-load-cookie)
                  (file-missing
                   (user-error "Define a cookie first with 'aoch-bootstrap' (or select 'Bootstrap Cookie' from the main menu.)")))))
    ;; It can't hurt to always re-store the cookie, even though one
    ;; might already be in place.
    (url-cookie-store "session" (get-hash cookie) nil ".adventofcode.com" "/")))

(defun aoch-prepare-puzzle (year day)
  "Download puzzle input for YEAR and DAY, possibly creating the
relevant directory.

The real work is done by `aoch--prepare-puzzle', which see."
  (interactive (aoch--get-year-and-day-from-user))
  (aoch--prepare-puzzle year day))

;; The cookie can't be made visible outside the callback. This is a
;; problem when invoking the puzzle-preparation code for the first
;; time after starting Emacs, when no cookies are loaded yet. The
;; code effectively has to be run once to hit a wall (a HTTP 400
;; error), upon which the cookie can then be set, upon which the
;; code can be successfully run again.
(defun aoch--prepare-puzzle (year day &optional try-again-p)
  "TRY-AGAIN-P marks that the function is inside a recursive call,
so that, in case an unfixable HTTP 400 is reached, it doesn't
fall into an infinite recursion."
  ;; In case the public interface wasn't run interactively.
  (when (< year 2015)
    (user-error "Advent of Code didn't exist before this time"))
  (url-retrieve (format "https://adventofcode.com/%d/day/%d/input" year day)
                (lambda (status)
                  (pcase (cl-third (plist-get status :error))
                    ((pred null)
                     ;; Delete the HTTP response header
                     (re-search-forward "^$" nil t)
                     (delete-region (point-min) (point))
                     ;; Prepare the puzzle directory before writing
                     ;; the input file
                     (let ((puzzle-directory (aoch--get-puzzle-directory year day)))
                       (make-directory puzzle-directory 'create-missing-parent-dirs)
                       (write-file (concat puzzle-directory "/input.txt"))))
                    (400
                     (if try-again-p
                         (error "Bad request (sorry, I don't know what to do here)")
                       ;; The cookie isn't being seen, but we can't
                       ;; load and store it before this callback
                       ;; either, so do it now, and try again.
                       (aoch--load-and-store-cookie)
                       (aoch--prepare-puzzle year day 'try-again)))
                    (404
                     (error "Puzzle hasn't been published yet"))
                    (500
                     (error "Bad cookie hash: run 'aoch-bootstrap' (or select 'Bootstrap Cookie' from the main menu.)"))))))

(defun aoch-visit-puzzle (year day)
  "Visit directory associated with a given YEAR and DAY (e.g.,
Day 12 of Year 2016)."
  (interactive (aoch--get-year-and-day-from-user))
  (let ((directory (aoch--get-puzzle-directory year day)))
    (unless (file-exists-p directory)
      (user-error "Prepare puzzle first with `aoch-prepare-puzzle'"))
    (dired directory)))

;;; Puzzle submission.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass aoch-solution (eieio-named eieio-persistent)
  ((level :initarg :level)
   (year :initarg :year)
   (day :initarg :day)
   (submitp :initform t
            :custom boolean
            :label "Submit this answer?"
            :documentation "Whether you wish to submit your answer to Advent of Code.

The default is 'on', which will both record your solution
locally, and submit it to Advent of Code. Set to 'off' to simply
record your solution."))
  :documentation "Advent of Code solution metadata.")

(define-inline aoch--get-solution-filename (year day level)
  "Return the string denoting the puzzle directory for a given YEAR
and DAY."
  (inline-quote (format "%s%d/day/%d/.part%d.eieio" aoch-top-level-directory ,year ,day ,level)))

(cl-defmethod initialize-instance :after ((solution aoch-solution) &rest slots)
  (with-slots (year day level) solution
    (oset solution file (aoch--get-solution-filename year day level))
    (oset solution file-header-line ";; -*- mode: lisp-data -*-")
    (oset solution do-backups nil)
    (oset solution object-name (format "Part %d Solution" level))))

;; These subclasses are needed for using :label and :documentation
;; fields that look nice in customization buffers.
(defclass aoch-solution-part-1 (aoch-solution)
  ((value :initarg :value
          :initform ""
          :custom string
          :label "Part 1"
          :documentation
          "Your Part 1 Solution."))
  :documentation "Advent of Code Part 1 solution.")

(defclass aoch-solution-part-2 (aoch-solution)
  ((value :initarg :value
          :initform ""
          :custom string
          :label "Part 2"
          :documentation
          "Your Part 2 Solution."))
  :documentation "Advent of Code Part 2 solution.")

(cl-defmethod eieio-done-customizing ((solution aoch-solution))
  (aoch-save solution)
  (when (oref solution submitp)
    (aoch-do-http-post solution (oref solution value))))

;; We need COLD-START to function as a kind of "static variable" - it
;; must keep track if the closing-over function hasn't been run yet
;; this Emacs session.
(let ((cold-start t))
  (cl-defmethod aoch-do-http-post ((solution aoch-solution) answer)
    (with-slots (year day level) solution
      (let* ((post-url (format "https://adventofcode.com/%s/day/%s/answer" year day))
             (post-data (format "level=%s&answer=%s" level answer))
             (url-request-method "POST")
             (url-request-data post-data)
             (url-request-extra-headers '(("Content-Type" . "application/x-www-form-urlencoded"))))
        ;; View POST request result in EWW upon fulfillment of the
        ;; request.
        ;;
        ;; There doesn't seem to be an error message associated with
        ;; the first failed cold-start, so we unconditionally try this
        ;; twice when cold-starting. After the first cold start, we
        ;; need only make the request once, as would be normal.
        (dotimes (_ (if cold-start 2 1))
          (aoch--load-and-store-cookie)
          (url-retrieve post-url (lambda (status)
                                   (eww "" nil (current-buffer)))))
        (setq cold-start nil)))))

(defun aoch--record-and-submit-part (level r1 r2)
  "Determine what we're customizing, and forward it to the acutal
customization code.

R1 and R2 are the region endpoints. An active region is used for
prefilling the :value field."
  (seq-let (year day) (aoch--inside-puzzle-directory-p)
    (let ((solution
           (cl-ecase level
             (1 (aoch-solution-part-1 :year year :day day :level level))
             (2 (aoch-solution-part-2 :year year :day day :level level)))))
      (when (region-active-p)
        (let ((selection (buffer-substring-no-properties r1 r2)))
          (oset solution value selection)))
      (eieio-customize-object solution))))

(defun aoch-record-and-submit-part-1 (r1 r2)
  "Record and possibly submit a Part 1 solution.

See `aoch--record-and-submit-part'."
  (interactive "r")
  (aoch--record-and-submit-part 1 r1 r2))

(defun aoch-record-and-submit-part-2 (r1 r2)
  "Record and possibly submit a Part 2 solution.

See `aoch--record-and-submit-part'."
  (interactive "r")
  (aoch--record-and-submit-part 2 r1 r2))

;;; Minor mode and menu.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar aoch-map (make-sparse-keymap))

(define-minor-mode aoch-mode
    "Activate certain niceties for working with Advent of Code.

The user is free to provide their own bindings for
`aoch-bootstrap', `aoch-prepare-puzzle', and
`aoch-visit-puzzle'. A GUI menu is also provided."
  :lighter " AOCH"
  :global t
  :keymap aoch-map)

(defun aoch--inside-puzzle-directory-p ()
  "Determine whether the user is visiting a puzzle directory, such
that the appropriate menu entries will become visible.

Return the puzzle's year and day, found in the puzzle directory
name."
  (when (string-match "\\([[:digit:]]+\\)/day/\\([[:digit:]][[:digit:]]?\\)" default-directory)
    (list (string-to-number (match-string-no-properties 1 default-directory))
          (string-to-number (match-string-no-properties 2 default-directory)))))

(defun aoch--do-nothing ()
  "Stub for non-functional menu entries."
  (interactive)
  (identity t))

(easy-menu-define nil aoch-map
  "Menu for Advent of Code Helper."
  '("Advent-of-Code"
    ["Bootstrap Cookie" aoch-bootstrap :help "Fix a broken cookie"]
    ["Prepare Puzzle" aoch-prepare-puzzle :help "Create directory and download puzzle input"]
    ["Visit Puzzle" aoch-visit-puzzle :help "Navigate to an existing puzzle"]
    ;; Submenus for Part submission.
    ("Part 1 Actions" :visible (aoch--inside-puzzle-directory-p)
     ["Record/Submit" aoch-record-and-submit-part-1 :help "Record and submit your Part 1 answer"])
    ("Part 2 Actions" :visible (aoch--inside-puzzle-directory-p)
     ["Record/Submit" aoch-record-and-submit-part-2 :help "Record and submit your Part 2 answer"])))

(provide 'aoc-helper)

;; Local Variables:
;; read-symbol-shorthands: (("aoch-" . "advent-of-code-helper-"))
;; End:
