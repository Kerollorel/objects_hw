program homework_main
  use kinds, only: rkind
  use vectors_mod
  use shapes_mod
  use polygons_mod
  use solids_mod
  implicit none

  ! Declarations (všetko musí byť deklarované pred spustiteľným kódom)
  type(vector2d) :: v1, v2, v3
  type(vector3d) :: vv3

  class(shape), allocatable :: s1, s2, s3
  type(rectangle) :: rec
  type(right_triangle) :: rt
  type(triangle_xy) :: tri
  type(quadrangle_xy) :: quad

  type(tetrahedron_xyz) :: tet
  class(solid), allocatable :: solid1

  ! -----------------------
  ! Spustiteľný kód
  ! -----------------------
  ! Task 1: vector sum test
  v1%x = 1.0_rkind; v1%y = 2.0_rkind
  v2%x = 3.0_rkind; v2%y = 4.0_rkind
  v3 = v1%sum(v2)
  print *, "Vector sum result (expect 4.0 6.0): ", v3%x, v3%y
  print *, "v1 norm (expect sqrt(5)) = ", v1%norm()

  ! vector3d test
  vv3%x = 1.0_rkind; vv3%y = 2.0_rkind; vv3%z = 2.0_rkind
  print *, "vector3d norm (expect 3.0) = ", vv3%norm()

  ! Task 2: shapes tests (alokujeme jednotlivé polymorfné objekty s rôznymi typmi)
  rec%width = 2.0_rkind; rec%height = 5.0_rkind
  allocate(s1, source=rec)

  rt%a = 3.0_rkind; rt%b = 4.0_rkind
  allocate(s2, source=rt)

  tri%x1 = 0.0_rkind; tri%y1 = 0.0_rkind
  tri%x2 = 1.0_rkind; tri%y2 = 0.0_rkind
  tri%x3 = 0.0_rkind; tri%y3 = 1.0_rkind
  allocate(s3, source=tri)

  print *, "----- Shape results -----"
  call print_shape(s1)
  call print_shape(s2)
  call print_shape(s3)

  ! Quadrangle test (rectangle corners)
  quad%x1 = 0.0_rkind; quad%y1 = 0.0_rkind
  quad%x2 = 2.0_rkind; quad%y2 = 0.0_rkind
  quad%x3 = 2.0_rkind; quad%y3 = 5.0_rkind
  quad%x4 = 0.0_rkind; quad%y4 = 5.0_rkind
  print *, "Quadrangle (rectangle corners) area (expect 10) = ", quad%area()
  print *, "Quadrangle perimeter (expect 14) = ", quad%perimeter()

  ! Tetrahedron test
  tet%x1 = 0.0_rkind; tet%y1 = 0.0_rkind; tet%z1 = 0.0_rkind
  tet%x2 = 1.0_rkind; tet%y2 = 0.0_rkind; tet%z2 = 0.0_rkind
  tet%x3 = 0.0_rkind; tet%y3 = 1.0_rkind; tet%z3 = 0.0_rkind
  tet%x4 = 0.0_rkind; tet%y4 = 0.0_rkind; tet%z4 = 3.0_rkind
  print *, "Tetrahedron volume (expect 0.5) = ", tet%volume()

  ! Polymorphic solid
  allocate(solid1, source=tet)
  print *, "Polymorphic solids(1) volume = ", solid1%volume()

contains

  subroutine print_shape(s)
    class(shape), intent(in) :: s
    select type(ss => s)
    class is (rectangle)
      print *, "Rectangle: area=", ss%area(), " perimeter=", ss%perimeter()
    class is (right_triangle)
      print *, "Right triangle: area=", ss%area(), " perimeter=", ss%perimeter()
    class is (triangle_xy)
      print *, "Triangle_xy: area=", ss%area(), " perimeter=", ss%perimeter()
    class default
      print *, "Unknown shape"
    end select
  end subroutine print_shape

end program homework_main
