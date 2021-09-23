(define-skeleton c++-header-skeleton
  "Inserts the boilerplate for a new C++ header file. Should be
run inside of a freshly created foo.h file, and will insert include
guards based on the file name.
For example, a filename of 'test.h' would result in

--
//TEST_H
#ifndef TEST_H
#define TEST_H

#endif //TEST_H
--

inserted into the file."
  nil ;;interactor, no prompt needed

  ;; test.h becomes TEST_H
  '(setq v1 (concat (upcase (file-name-base (buffer-file-name))) "_H"))

  "//" v1 "\n"
  "#ifndef " v1 "\n"
  "#define " v1 "\n\n"
  >_
  "#endif //" v1)
