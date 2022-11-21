(command "_.OSNAP" "_NONE")

(setq circle_dots_count 100)
(setq desirable_figures_gap 5)
(setq desirable_bottom_circles_gap 2)
(setq desirable_cylinder_lines_angle 15)
(setq desirable_triangle_lines_count 8)

(defun get_circle_dots (x r dots_count)
  (setq p 0)
  (setq step (/ (* 2 pi) dots_count))
  
  (setq result (list))
  (repeat (1+ dots_count)
    (setq y (* r (sin p)))
    (setq z (* r (cos p)))
    (setq p (+ p step))
    (setq result (append result (list (list x y z))))
  )
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

(defun print_holey_bottom (x r hr anglee)
  (setq circles_count (fix (/ (- r hr) desirable_bottom_circles_gap)))
  (setq circle_step (/ (- r hr) circles_count))
  (setq r_step (/ (- r hr) circles_count))
  
  (setq current_r r)
  (repeat (1+ circles_count)
    (print_circle x current_r)
    (setq current_r (- current_r r_step))
  )
  
  (setq lines_count (/ 360 anglee))
  (setq outer_ring_lines_dots (get_circle_dots x r lines_count))
  (setq inner_ring_lines_dots (get_circle_dots x hr lines_count))
  (repeat lines_count
    (command "_3dpoly" (car outer_ring_lines_dots) (car inner_ring_lines_dots) "")
    (setq outer_ring_lines_dots (cdr outer_ring_lines_dots))
    (setq inner_ring_lines_dots (cdr inner_ring_lines_dots))
  )
)

(defun print_cylinder+ (x rb re h)
  (setq circles_count (fix (/ h desirable_figures_gap)))
  (setq x_step (/ h circles_count 1.))
  (setq r_step (/ (- re rb) circles_count 1.))
  
  (setq current_x x)
  (setq current_r rb)
  (repeat (1+ circles_count)
    (print_circle current_x current_r)
    (setq current_x (+ current_x x_step))
    (setq current_r (+ current_r r_step))
  )
  
  (setq lines_count (/ 360 desirable_cylinder_lines_angle))
  (setq first_circle_dots (get_circle_dots x rb lines_count))
  (setq last_circle_dots (get_circle_dots (+ x h) re lines_count))
  (repeat lines_count
    (command "_3dpoly" (car first_circle_dots) (car last_circle_dots) "")
    (setq first_circle_dots (cdr first_circle_dots))
    (setq last_circle_dots (cdr last_circle_dots))
  )
)

(defun print_triangle (x r)
  (setq triangle_dots (get_circle_dots x r 3))
  (print_figure triangle_dots)
)


(defun get_triangle_lines_dots(dot1 dot2 dot3)
  (print dot1)
  (print dot2)
  (print dot3)
  
  (setq x (car dot1))
  (setq result (list nil))
  (setq dot_pairs (list (list dot1 dot2) (list dot2 dot3) (list dot3 dot1)))
  
  (foreach dot_pair dot_pairs
    (setq d1 (car dot_pair))
    (setq d2 (cadr dot_pair))
    (setq y_step (/ (- (cadr d2) (cadr d1)) desirable_triangle_lines_count))
    (setq z_step (/ (- (caddr d2) (caddr d1)) desirable_triangle_lines_count))
    
    (setq current_y (cadr d1))
    (setq current_z (caddr d1))
    (repeat desirable_triangle_lines_count
      (setq current_y (+ current_y y_step))
      (setq current_z (+ current_z z_step))
      (setq result (append result (list (list x current_y current_z))))
    )
  )
  (cdr result)
)

(defun print_pyramid (x r h)
  (setq triangles_count (fix (/ h desirable_figures_gap)))
  (setq triangle_step (/ h triangles_count))
  (setq r_step (/ r triangles_count 1.0))
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
  
  (setq triangle_lines_dots (get_triangle_lines_dots (car triangle_dots) (cadr triangle_dots) (caddr triangle_dots)))
  (foreach d triangle_lines_dots
    (command "_3dpoly" d second_dot "")
  )
)

(defun c:figure ()
  (setq x -30 cr 30 ch 50 ph 50)
  (print_holey_bottom x cr (* cr 0.75) desirable_cylinder_lines_angle)
  (print_cylinder+ x (* cr 0.75) 0 (* ch 0.5))
  (print_cylinder+ x cr cr ch)
  (print_pyramid (+ x ch) (/ cr 2) ph)
  (print_holey_bottom (+ x ch) cr (/ cr 2) 120)
  (princ)
)
