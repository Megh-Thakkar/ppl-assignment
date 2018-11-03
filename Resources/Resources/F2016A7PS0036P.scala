package pplAssignment

object F2016A7PS0036P{

    def dotProduct(lis1: List[List[Double]], lis2: List[List[Double]]): Double = {
        def listProduct(l1: List[Double], l2: List[Double]): Double = {
            if (l1.isEmpty || l2.isEmpty) {
                0;
            } else {
                (l1.head*l2.head) + listProduct(l1.tail, l2.tail);
            }
        }
        if (lis1.isEmpty || lis2.isEmpty || lis1.head.isEmpty || lis2.head.isEmpty) {
            0;
        } else {
            listProduct(lis1.head, lis2.head) + dotProduct(lis1.tail, lis2.tail);
        }
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
            if (imageSize.head < kernelSize.head || imageSize.tail.head < kernelSize.tail.head) {
                List[Double]();
            } else {
                val trimmed = trimColumn(Image);
                dotProduct(Image, Kernel) :: convoluteRow(trimmed, Kernel, List(imageSize.head, imageSize.tail.head-1), kernelSize);
            }
        }

        if (imageSize.head < kernelSize.head || imageSize.tail.head < kernelSize.tail.head) {
            List[List[Double]]();
        } else {
            convoluteRow(Image, Kernel, imageSize, kernelSize) :: convolute(Image.tail, Kernel, List(imageSize.head-1, imageSize.tail.head), kernelSize);
        }
    }

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
        if (Image.isEmpty || Image.head.isEmpty) {
            List[Double]();
        } else {
            poolingFunc(poolToList(Image, K, K)) :: singlePooling(poolingFunc, removeKColumns(Image, K), K);
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

    def poolingLayer(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int) : List[List[Double]] = {
        if (Image.isEmpty) {
            List[List[Double]]();
        } else {
            singlePooling(poolingFunc, Image, K) :: poolingLayer(poolingFunc, removeKRows(Image, K), K);
        }
    }

    def mixedLayer(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], 
        kernelSize:List[Int], activationFunc:Double => Double, 
        poolingFunc:List[Double]=>Double, K:Int) : List[List[Double]] = {
            poolingLayer(poolingFunc, 
                        activationLayer(activationFunc, 
                                        convolute(Image, 
                                                Kernel, imageSize, kernelSize
                                    )
                                ),
                            K
                        );
        }

    def max(x:Double, y:Double) : Double = {
        if (x>y) x;
        else y;
    }

    def min(x:Double, y:Double) : Double = {
        if (x>y) y;
        else x;
    }

    def findMaxRow(list:List[Double]) : Double = {
        if (list.tail.isEmpty){
            list.head;
        } else {
            max(list.head, findMaxRow(list.tail));
        }
    }

    def findMinRow(list:List[Double]) : Double = {
        if (list.tail.isEmpty){
            list.head;
        } else {
            min(list.head, findMinRow(list.tail));
        }
    }

    def maxList(Image:List[List[Double]]) : List[Double] = {
        if (Image.isEmpty || Image.head.isEmpty) {
            List[Double]();
        } else {
            findMaxRow(Image.head) :: maxList(Image.tail);
        }
    }

    def minList(Image:List[List[Double]]) : List[Double] = {
        if (Image.isEmpty || Image.head.isEmpty) {
            List[Double]();
        } else {
            findMinRow(Image.head) :: minList(Image.tail);
        }
    }

    def findMax(Image:List[List[Double]]) : Double = {
        findMaxRow(maxList(Image));
    }

    def findMin(Image:List[List[Double]]) : Double = {
        findMinRow(minList(Image));
    }

    def normalisedValue(x:Double, min_v:Double, max_v:Double) : Int = {
        Math.round(((x - min_v)/(max_v - min_v))*255).toInt;
    }

    def normaliseHelper(Image:List[List[Double]], min_v:Double, max_v:Double) : List[List[Int]] = {
        def normaliseRow(row:List[Double], min_v:Double, max_v:Double) : List[Int] = {
            if (row.isEmpty){
                List[Int]();
            } else {
                normalisedValue(row.head, min_v, max_v) :: normaliseRow(row.tail, min_v, max_v);
            }
        }

        if (Image.isEmpty || Image.head.isEmpty) {
            List[List[Int]]();
        } else {
            normaliseRow(Image.head, min_v, max_v) :: normaliseHelper(Image.tail, min_v, max_v);
        }
    }

    def normalise(Image:List[List[Double]]) : List[List[Int]] = {
        val min_v:Double = findMin(Image);
        val max_v:Double = findMax(Image);
        normaliseHelper(Image, min_v, max_v);
    }

    def ReLuActivation(value:Double) : Double = {
        max(0, value);
    }

    def leakyReLuActivation(value:Double) : Double = {
        if (value > 0) value;
        else 0.5*value;
    }

    def rowSum(row:List[Double]) : Double = {
        if (row.isEmpty) 0;
        else {
            row.head + rowSum(row.tail);
        }
    }

    def findLength(mat:List[Double]) : Double = {
        if (mat.isEmpty) {
            0;
        } else {
            1 + findLength(mat.tail);
        }
    }

    def avgPooling(mat:List[Double]) : Double = {
        rowSum(mat)/findLength(mat);
    }

    def maxPooling(mat:List[Double]) : Double = {
        findMaxRow(mat);
    }

    def scalarAddition(mat:List[List[Double]], bias:Double) : List[List[Double]] = {
        def rowMultiplication(row:List[Double], bias:Double) : List[Double] = {
            if (row.isEmpty) {
                List[Double]();
            } else {
                (row.head + bias) :: rowMultiplication(row.tail, bias);
            }
        }
        
        if (mat.isEmpty || mat.head.isEmpty) {
            List[List[Double]]();
        } else {
            rowMultiplication(mat.head, bias) :: scalarAddition(mat.tail, bias);
        }
    }

    def scalarMultiplication(mat:List[List[Double]], weight:Double) : List[List[Double]] = {
        def rowMultiplication(row:List[Double], weight:Double) : List[Double] = {
            if (row.isEmpty) {
                List[Double]();
            } else {
                row.head*weight :: rowMultiplication(row.tail, weight);
            }
        }

        if (mat.isEmpty || mat.head.isEmpty) {
            List[List[Double]]();
        } else {
            rowMultiplication(mat.head, weight) :: scalarMultiplication(mat.tail, weight);
        }
    }

    def matrixAddition(mat1:List[List[Double]], mat2:List[List[Double]]) : List[List[Double]] = {
        def rowAddition(row1:List[Double], row2:List[Double]) : List[Double] = {
            if (row1.isEmpty || row2.isEmpty) List[Double]();
            else {
                (row1.head + row2.head) :: rowAddition(row1.tail, row2.tail);
            }
        }
        if (mat1.isEmpty || mat2.isEmpty || mat1.head.isEmpty || mat2.head.isEmpty) List[List[Double]]();
        else {
            rowAddition(mat1.head, mat2.head) :: matrixAddition(mat1.tail, mat2.tail);
        }
    }

    def assembly(Image:List[List[Double]],
            imageSize:List[Int],
            w1:Double,
            w2:Double,
            b:Double,
            Kernel1:List[List[Double]],
            kernelSize1:List[Int],
            Kernel2:List[List[Double]],
            kernelSize2:List[Int],
            Kernel3:List[List[Double]],
            kernelSize3:List[Int],
            Size: Int
        ) : List[List[Int]] = {
        
        val temp_output_1 = mixedLayer(
            Image, 
            Kernel1, 
            imageSize, 
            kernelSize1, 
            ReLuActivation,
            avgPooling, 
            Size
        );
        val temp_output_2 = mixedLayer(
            Image, 
            Kernel2, 
            imageSize, 
            kernelSize2, 
            ReLuActivation,
            avgPooling, 
            Size
        );

        val temp_output_3 = scalarAddition(
            matrixAddition(
                scalarMultiplication(temp_output_1, w1),
                scalarMultiplication(temp_output_2, w2)
            ), 
            b
        );
        val new_list : List[Int] = List(((imageSize.head-kernelSize1.head+1)/Size), ((imageSize.tail.head-kernelSize2.tail.head+1)/Size));
        val temp_output_4 = mixedLayer(
            temp_output_3,
            Kernel3,
            new_list,
            kernelSize3,
            leakyReLuActivation,
            maxPooling,
            Size
        );
        normalise(temp_output_4);
    }
}