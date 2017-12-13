; ModuleID = 'Pixelman'
source_filename = "Pixelman"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@s = private unnamed_addr constant [15 x i8] c"\22Hello World!\22\00"

declare i32 @printf(i8*, ...)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.1, i32 0, i32 0), i8* getelementptr inbounds ([15 x i8], [15 x i8]* @s, i32 0, i32 0))
  ret i32 0
}
