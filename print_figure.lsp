(command "_.OSNAP" "_NONE")

(setq circle_dots_count 360)
(setq desirable_figures_gap 10)
(setq desirable_cylinder_lines_angle 20)

;возвращает список точек окружности с равным шагом
(defun get_circle_dots (x r dots_count)
  (setq p 0)
  (setq step (/ (* 2 pi) dots_count))
  
  (setq result (list nil))
  (repeat (1+ dots_count)
    (setq y (* r (sin p)))
    (setq z (* r (cos p)))
    (setq result (append result (list (list x y z))))
    (setq p (+ p step))
  )
  (cdr result)
)

(defun print_figure (dots_list)
  (command "_3dpoly")
  (foreach dot dots_list
    (command dot)
  )
  (command "")
)

(defun print_circle (x r)
  (setq circle_dots (get_circle_dots x r circle_dots_count))
  (print_figure circle_dots)
)

(defun print_cylinder (x r h)
  ;желательное расстояние между кольцами
  (setq circles_count (fix (/ h desirable_figures_gap)))
  (setq circle_step (/ h circles_count))
  
  (setq current_x x)
  (repeat (1+ circles_count)
    (print_circle current_x r)
    (setq current_x (+ current_x circle_step))
  )
  
  (setq lines_count 15)
  (setq circle_dots (get_circle_dots x r lines_count))
  (foreach dot circle_dots
    (setq second_dot (list (+ (car dot) h) (cadr dot) (caddr dot)))
    (command "_3dpoly" dot second_dot "")
  )
)

(defun print_triangle (x r)
  (setq triangle_dots (get_circle_dots x r 3))
  (print_figure triangle_dots)
)

(defun print_pyramid (x r h)
  (setq triangles_count (fix (/ h desirable_figures_gap)))
  (setq triangle_step (/ h triangles_count))
  (setq r_step (/ r triangles_count))
  (setq current_x x current_r r)
  (repeat triangles_count
    (print_triangle current_x current_r)
    (setq current_x (+ current_x triangle_step))
    (setq current_r (- current_r r_step))
  )
  
  (setq triangle_dots (get_circle_dots x r 3))
  (setq second_dot (list (+ x h) 0 0))
  (foreach dot triangle_dots
    (command "_3dpoly" dot second_dot "")
  )
)

(setq x -30 cr 30 ch 50 ph 50)
(print_cylinder x cr ch)
(print_pyramid (+ x ch) (* cr 0.75) ph)
