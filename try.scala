// def Factorial(a:Int):Int = {
//     if(a == 1) // Base condition
//         a;
//     else
//         a*Factorial(a-1);
// }

// println (Factorial(4));
// println (Factorial(5));
// println (Factorial(6));

// def listProduct(l1: List[Double], l2: List[Double]): Double = l2 match {
//         case Nil => 0
//         case x::xs => x*l1.head + listProduct(l1.tail, xs)
// }

// def dotProduct(lis1: List[List[Double]], lis2: List[List[Double]]): Double = lis2 match {
//     case Nil => 0
//     case x::xs => listProduct(lis1.head, x) + dotProduct(lis1.tail, xs)
// }

// def convoluteHelper(lis1: List[Double], left: Int, main: List[List[[Double]], kernel: List[List[Double]]): List[Double] = left match{
//     case 0 => lis1
//     case _ => {
//         append(lis1, dotProduct(main, kernel));
//         convoluteHelper(lis1, left-1, main.tail, kernel)
//     }
// }

// def convoluteRows(matrix:List[List[Double]], Image:List[List[Double]], Kernel:List[List[Double]], rows, cols) : List[List[Double]] = Image match{
    
// }

// def convolute(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int]) : List[List[Double]] = {
//     val rows: Double = imageSize.head - kernelSize.head + 1;
//     val cols: Double = imageSize.tail.head - kernelSize.tail.head + 1;
//     val matrix: List[List[Double]];
//     matrix = convoluteRows(matrix, Image, Kernel, rows, cols);
// }

// def trimColumn(Image:List[List[Double]]) : List[List[Double]] = {
//     if (Image.isEmpty) {
//         List[List[Double]]();
//     } else {
//         Image.head.tail :: trimColumn(Image.tail);
//     }
// }

// def convolute(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int]) : List[List[Double]] = {
    
//     def convoluteRow(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int]) : List[Double] = {
//         println (Image);
//         println (imageSize);
//         if (imageSize.head < kernelSize.head || imageSize.tail.head < kernelSize.tail.head) {
//             println ("Empty");
//             List[Double]();
//         } else {
//             val trimmed = trimColumn(Image);
//             println ("###")
//             println (dotProduct(Image, Kernel));
//             dotProduct(Image, Kernel) :: convoluteRow(trimmed, Kernel, List(imageSize.head, imageSize.tail.head-1), kernelSize);
//         }
//     }

//     if (imageSize.head < kernelSize.head || imageSize.tail.head < kernelSize.tail.head) {
//         List[List[Double]]();
//     } else {
//         convoluteRow(Image, Kernel, imageSize, kernelSize) :: convolute(Image.tail, Kernel, List(imageSize.head-1, imageSize.tail.head), kernelSize);
//     }
// }


// val lis1: List[List[Double]] =
//     List(
//         List(1, 0),
//         List(-1, 0),
//     );

// val lis2: List[List[Double]] =
//     List(
//         List(0, -1),
//         List(-1, 0)
//     );

// val Image: List[List[Double]] =
//     List(
//         List(1, 1, 1, 0, 0),
//         List(0, 1, 1, 1, 0),
//         List(0, 0, 1, 1, 1),
//         List(0, 0, 1, 1, 0),
//         List(0, 1, 1, 0, 0)
//     );

// val Kernel: List[List[Double]] =
//     List(
//         List(1, 0, 1),
//         List(0, 1, 0),
//         List(1, 0, 1),
//     );

// val imageSize: List[Int] = List(5, 5);
// val kernelSize: List[Int] = List(3, 3);

// val tl: List[Int] = List();
// val i: Int = 4;
// val j: Int = 5;
// val t2: List[Int] = tl ::: List(i);
// val t3: List[Int] = t2 ::: List(j);
// println (List.concat(tl, List(i)));
// List.concat(tl, List(j));
// tl = tl ::: List(i);
// tl = tl ::: List(j);
// println  (dotProduct(lis1, lis2))

// println (convolute(Image, Kernel, imageSize, kernelSize));
// println (trimColumn(Image));

// println (dotProduct(Image, Kernel));

// def activationFunc(inp:Double) : Double = {
//     if (inp > 5){
//         1.0;
//     } else {
//         0.0;
//     }
// }

// def activationLayer(activationFunc:Double => Double, Image:List[List[Double]]) : List[List[Double]] = {

//     def activationHelper(activationFunc:Double => Double, imageRow:List[Double]) : List[Double] = {
//         if (imageRow.isEmpty) {
//             List[Double]();
//         } else {
//             activationFunc(imageRow.head) :: activationHelper(activationFunc, imageRow.tail);
//         }
//     }

//     if (Image.isEmpty) {
//         List[List[Double]]();
//     } else {
//         activationHelper(activationFunc, Image.head) :: activationLayer(activationFunc, Image.tail);
//     }

// }

def poolToList(matrix:List[List[Double]], k:Int) : List[Double] = {

    def rowToList(row:List[Double], r:Int) : List[Double] = {
        if (row.isEmpty || r<=0) {
            List[Double]();
        } else {
            row.head :: rowToList(row.tail, r-1);
        }
    }
    
    if (matrix.isEmpty) {
        List[Double]();
    } else {
        rowToList(matrix.head, k) ::: poolToList(matrix.tail, k);
    }
}

def removeKColumns(matrix:List[List[Double]], k:Int) : List[List[Double]] = {

    def removeKElements(row:List[Double], remaining:Int) : List[Double] = {
        if (row.isEmpty) {
            List[Double]();
        } else {
            if (remaining <= 0) {
                row.head :: removeKElements(row.tail, remaining-1);
            } else {
                removeKElements(row.tail, remaining-1);
            }
        }
    }

    if (matrix.isEmpty) {
        List[List[Double]]();
    } else {
        removeKElements(matrix.head, k) :: removeKColumns(matrix.tail, k);
    }
}

def removeKRows(matrix:List[List[Double]], k:Int) : List[List[Double]] = {
    if (matrix.isEmpty) {
        List[List[Double]]();
    } else {
        if (k<=0) {
            matrix.head :: removeKRows(matrix.tail, k-1);
        } else {
            removeKRows(matrix.tail, k-1);
        }
    }
}

val Image: List[List[Double]] =
    List(
        List(10, 9, 4, 3, 7),
        List(5, 6, 1, 2, 9),
        List(0, 7, 6, 5, 10),
        List(7, 8, 1, 1, 4),
        List(0, 1, 1, 7, 7)
    );

// println (removeKColumns(Image, 2));

def poolingFunc(lis1:List[Double]) : Double = {
    if (lis1.isEmpty) {
        0;
    } else {
        lis1.head + poolingFunc(lis1.tail);
    }
}

def singlePooling(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int) : List[Double] = {
    if (Image.head.isEmpty) {
        List[Double]();
    } else {
        // println (Image.isEmpty);
        // println(poolToList(Image, K));
        poolingFunc(poolToList(Image, K)) :: singlePooling(poolingFunc, removeKColumns(Image, K), K);
    }
}

// println (poolToList(Image, 3, 3));

// def removeKElements(row:List[Double], remaining:Int) : List[Double] = {
//     if (row.isEmpty) {
//         List[Double]();
//     } else {
//         if (remaining <= 0) {
//             row.head :: removeKElements(row.tail, remaining-1);
//         } else {
//             removeKElements(row.tail, remaining-1);
//         }
//     }
// }

val test: List[Double] = List(1,2,3,4,5,6,7,8,9);

val testPool: List[List[Double]] =
    List(
        List(10, 9, 4, 3, 7, 12, 1, 2, 3),
        List(5, 6, 1, 2, 9, 11, 4, 5, 6),
        List(8, 9, 10, 2, 3, 1, 7, 8, 9),
    );

println (singlePooling(poolingFunc, testPool, 3));