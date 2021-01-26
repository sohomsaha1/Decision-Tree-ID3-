# Decision-Tree-ID3-
Decision Tree using BST |  Implementing ID3 ML algorithm.

## This is a decision tree which looks at a big Database of animals.

Different Characteristics of these animals lead this program to identify most likely what this is.



***How ID3 works...***
1. We first collect the different attributes
2. We collect the entropy (uncertainty associated with a given attribute)
3. Determine attribute with minimum entropy 
4. Create a classifier to make a decision based off augmented entropies



ID3 Psuedocode (Credit to Wikipedia 2021)

***ID3 (Examples, Target_Attribute, Attributes)***
    
    Create a root node for the tree
    If all examples are positive, Return the single-node tree Root, with label = +.
    If all examples are negative, Return the single-node tree Root, with label = -.
    If number of predicting attributes is empty, then Return the single node tree Root,
    with label = most common value of the target attribute in the examples.
    Otherwise Begin
        A ← The Attribute that best classifies examples.
        Decision Tree attribute for Root = A.
        For each possible value, vi, of A,
            Add a new tree branch below Root, corresponding to the test A = vi.
            Let Examples(vi) be the subset of examples that have the value vi for A
            If Examples(vi) is empty
                Then below this new branch add a leaf node with label = most common target value in the examples
            Else below this new branch add the subtree ID3 (Examples(vi), Target_Attribute, Attributes – {A})
    End
    Return Root
    

### Example of Decision Tree

![](images/BST.png)

    
 
 
