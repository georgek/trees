(defparameter tg (make-gamma-graph (list (cord 'a 'b 2) (cord 'a 'c 1) (cord 'b 'c 2) (cord 'c 'd 3) (cord 'd 'e 1) (cord 'd 'f 3) (cord 'd 'g 8) (cord 'e 'f 2) (cord 'f 'h 6) (cord 'g 'h 3) (cord 'g 'i 4))))

(defparameter bg (make-gamma-graph (list (cord 'a 'b 2) (cord 'b 'c 4) (cord 'a 'e 6) (cord 'c 'e 6) (cord 'd 'e 2) (cord 'c 'f 8) (cord 'b 'g 8) (cord 'a 'h 12) (cord 'f 'h 12) (cord 'c 'i 12) (cord 'f 'i 12) (cord 'd 'j 12) (cord 'e 'j 12) (cord 'g 'j 12) (cord 'h 'j 10) (cord 'b 'k 12) (cord 'i 'k 10))))

