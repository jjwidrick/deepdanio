Genetic disorders affecting skeletal muscle function, such as muscular dystrophies and myopathies, result in weakness and impaired mobility. Zebrafish (*Danio rerio*) with similar mutations are an important pre-clinical model for developing therapies that target these diseases. It is therefore critically important to quantify the mobility of these models as a means of evaluating the efficacy of potential therapeutic treatments.

We used the open source toolkit DeepLabCut to develop a neural network for rapidly and accurately identifying anatomical keypoints on swimming zebrafish larvae (see Mathis et al. Nat Neurosci 21, 1281–1289, 2018, and 
Nath et al. Nat Protoc 14, 2152–2176, 2019, for general information about DeepLabCut). The pose estimates produced by DeepLabCut consist of keypoint x- and y-coordinates in .csv format. 

The deepdanio package converts these x- and y-coordinates into kinematic variables that mathematically describe the larva's movement. Further details are available at [Widrick et al.](https://www.biorxiv.org/content/10.1101/2024.12.05.627004v1).
