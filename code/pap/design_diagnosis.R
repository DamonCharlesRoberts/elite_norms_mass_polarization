# Title: Elite norms and mass polarization study 3 design diagnosis ----

# Notes: ----
    #* Description: script for diagnosing experimental design used in study 3 of Elite norms and mass polarization project ----
    #* Updated: 2022 - 02 - 16 ----
    #* Updated by: dcr ----

# Setup ----
    #* modularly load packages ----
#install.packages('box')#install box package, if not already installed, to modularly load functions for packages
box::use(
    DeclareDesign = DeclareDesign[...],
    DesignLibrary = DesignLibrary[block_cluster_two_arm_designer, cluster_sampling_designer]
    )

# Declare the Design ----
    #* Define the population ----
study3Design = function(designType, sampleSize, blocks, clusters, ate){
    if(designType == 'block') {
        blockClusterDesign = block_cluster_two_arm_designer(
            N_blocks = blocks,
            N_clusters_in_block = clusters,
            N_i_in_cluster = sampleSize/(blocks*clusters),
            ate = ate
        )
    } else {
        factorialDesign2x3 = cluster_sampling_designer(
            N_blocks = blocks,
            N_clusters_in_block = clusters,
            N_i_in_cluster = sampleSize/blocks,
            n_clusters_in_block = clusters,
            n_i_in_cluster = sampleSize/(blocks*cluster),
            ate = ate
        )
    }
}

blockedDesign = study3Design(designType = 'blocks', sampleSize = 2000, blocks = 2, clusters = 6, ate = .3)