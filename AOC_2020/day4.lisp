(defparameter *input4* (uiop:read-file-lines "~/Documents/Lisp/input4"))

(defun zip (input)
  (let ((newlist nil) (next-item ""))
    (dolist (item input)
      (if (> (length item) 0) (setf next-item (concatenate 'string next-item " " item))
          (list (push next-item newlist) (setf next-item ""))))
    (push next-item newlist)
    (print newlist)))

(defun parse-field (passport field)
  (let ((subfield (subseq passport (+ (search field passport) 4))))
       (subseq subfield 0 (position #\  subfield))))

(defun byr-valid (passport)
  (if (= 4 (length (parse-field passport "byr")))
      (let ((field-value (parse-integer (parse-field passport "byr"))))
        (if (and (>= field-value 1920) (<= field-value 2002)) t nil)) nil))

(defun iyr-valid (passport)
  (if (= 4 (length (parse-field passport "iyr")))
      (let ((field-value (parse-integer (parse-field passport "iyr"))))
        (if (and (>= field-value 2010) (<= field-value 2020)) t nil)) nil))

(defun eyr-valid (passport)
  (if (= 4 (length (parse-field passport "eyr")))
      (let ((field-value (parse-integer (parse-field passport "eyr"))))
        (if (and (>= field-value 2020) (<= field-value 2030)) t nil)) nil))

(defun cm-valid (hgt-field)
  (let ((height (subseq hgt-field 0 3)))
    (if (every 'digit-char-p height) 
        (if (and 
              (>= (parse-integer height) 150)
              (<= (parse-integer height) 193)) t nil)
        nil)))

(defun in-valid (hgt-field)
  (let ((height (parse-integer (subseq hgt-field 0 2))))
    (if (and (>= height 59) (<= height 76)) t nil)))

(defun hgt-valid (passport)
  (let ((field-value (parse-field passport "hgt")))
    (cond
      ((search "cm" field-value) (and (= 5 (length field-value)) (cm-valid field-value)))
      ((search "in" field-value) (and (= 4 (length field-value)) (in-valid field-value))))))

(defun hcl-char-valid (hcl-char)
  (if (position hcl-char "0123456789abcdef") t nil))

(defun hcl-valid (passport)
  (let ((field-value (parse-field passport "hcl")))
    (and
      (position #\# field-value)
      (every 'hcl-char-valid (subseq field-value 1))
      (= 7 (length field-value)))))

(defun ecl-valid (passport)
  (let ((field-value (parse-field passport "ecl")))
    (if (search field-value "amb blu brn gry grn hzl oth") 1 0)))

(defun pid-valid (passport)
  (let ((field-value (parse-field passport "pid")))
    (and (= 9 (length field-value))
         (every 'digit-char-p field-value))))

(defun evaluate-passport (passport)
  (if 
      (and
        (search "byr:" passport)
        (search "iyr:" passport)
        (search "eyr:" passport)
        (search "hgt:" passport)
        (search "hcl:" passport)
        (search "ecl:" passport)
        (search "pid:" passport))
      passport))

(defun pre-validate (input)
  (remove nil (map 'list 'evaluate-passport (zip input))))

(defun evaluate-passport-2 (passport)
  (if 
      (and
        (byr-valid passport)
        (iyr-valid passport)
        (eyr-valid passport)
        (hgt-valid passport)
        (hcl-valid passport)
        (ecl-valid passport)
        (pid-valid passport))
      1 0))
