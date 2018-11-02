package pplAssignment

object <STUDENT_ID>{
    //Start Coding from here

    def listProduct(l1: List[Double], l2: List[Double]): Double = l2 match {
        case Nil => 0
        case x::xs => x*l1.head + listProduct(l1.tail, xs)
    }

    def dotProduct(lis1: List[List[Double]], lis2: List[List[Double]]): Double = lis2 match {
        case Nil => 0
        case x::xs => listProduct(lis1.head, x) + dotProduct(lis1.tail, xs)
    }

    def trimColumn(Image:List[List[Double]]) : List[List[Double]] = {
        if (Image.isEmpty) {
            List[List[Double]]();
        } else {
            Image.head.tail :: trimColumn(Image.tail);
        }
    }

    def convolute(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int]) : List[List[Double]] = {
        
        def convoluteRow(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int]) : List[Double] = {
            // println (Image);
            // println (imageSize);
            if (imageSize.head < kernelSize.head || imageSize.tail.head < kernelSize.tail.head) {
                // println ("Empty");
                List[Double]();
            } else {
                val trimmed = trimColumn(Image);
                // println ("###")
                // println (dotProduct(Image, Kernel));
                dotProduct(Image, Kernel) :: convoluteRow(trimmed, Kernel, List(imageSize.head, imageSize.tail.head-1), kernelSize);
            }
        }

        if (imageSize.head < kernelSize.head || imageSize.tail.head < kernelSize.tail.head) {
            List[List[Double]]();
        } else {
            convoluteRow(Image, Kernel, imageSize, kernelSize) :: convolute(Image.tail, Kernel, List(imageSize.head-1, imageSize.tail.head), kernelSize);
        }
    }

    // def testActivationFunc(inp:Double) : Double = {
    //     if (inp > 5){
    //         1.0;
    //     } else {
    //         0.0;
    //     }
    // }

    def activationLayer(activationFunc:Double => Double, Image:List[List[Double]]) : List[List[Double]] = {

        def activationHelper(activationFunc:Double => Double, imageRow:List[Double]) : List[Double] = {
            if (imageRow.isEmpty) {
                List[Double]();
            } else {
                activationFunc(imageRow.head) :: activationHelper(activationFunc, imageRow.tail);
            }
        }

        if (Image.isEmpty) {
            List[List[Double]]();
        } else {
            activationHelper(activationFunc, Image.head) :: activationLayer(activationFunc, Image.tail);
        }

    }

    def poolToList(matrix:List[List[Double]], k:Int, left:Int) : List[Double] = {

        def rowToList(row:List[Double], r:Int) : List[Double] = {
            if (row.isEmpty || r<=0) {
                List[Double]();
            } else {
                row.head :: rowToList(row.tail, r-1);
            }
        }
        
        if (matrix.isEmpty || left<=0) {
            List[Double]();
        } else {
            rowToList(matrix.head, k) ::: poolToList(matrix.tail, k, left-1);
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

    def singlePooling(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int) : List[Double] = {
        if (Image.head.isEmpty) {
            List[Double]();
        } else {
            // println (Image.isEmpty);
            // println(poolToList(Image, K, K));
            poolingFunc(poolToList(Image, K, K)) :: singlePooling(poolingFunc, removeKColumns(Image, K), K);
        }
    }

    def poolingLayer(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int) : List[List[Double]] = {
        if (Image.isEmpty) {
            List[List[Double]]();
        } else {
            singlePooling(poolingFunc, Image, K) :: poolingLayer(poolingFunc, removeKRows(Image, K), K);
        }
    }
}