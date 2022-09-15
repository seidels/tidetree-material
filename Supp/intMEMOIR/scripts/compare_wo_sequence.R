library(ggplot2)

# img
text_size= 25
height = 10
width = 15
font = "Arial"


dat_no_seq = "../../../Fig3/2_inference/inference_logs/noSequence/inference_noSequence.combined.log"
dat_no_seq = read.delim(dat_no_seq)
dat_no_seq$id = "W/o sequence"

dat_seq = "../../../Fig3/2_inference/inference_logs/priorInfoOnScarring/all_trees_rho_origin_reparScarClock_treeInit25_PriorOnScarringRatesI_scarringStart_clock_ExpPriorScarringStart_combined.log"
dat_seq = read.delim(dat_seq)
dat_seq$id = "With sequence"

birth = rbind(dat_seq[, c("birthRate", "id")],
              dat_no_seq[, c("birthRate", "id")])

p = ggplot(birth, aes(x=id, y=birthRate)) +
  geom_violin()+
  theme_bw() +
  ylim(0, 0.45)+
  xlab("")+ theme(text=element_text(
    size = text_size ))

ggsave(filename = "../birth_rate.pdf",
       plot = p, height = height, width = width, units = "cm",
)



death = rbind(dat_seq[, c("deathRate", "id")],
              dat_no_seq[, c("deathRate", "id")])

p = ggplot(death, aes(x=id, y=deathRate)) +
  geom_violin()+
  theme_bw() +
  ylim(0, 0.45)+
  xlab("")+
  theme(text=element_text( size = text_size ))
p
ggsave(filename = "../death_rate.pdf",
       plot = p, height = height, width = width, units = "cm"
)


#sum stats
quantile_birth_noseq = quantile(dat_no_seq$birthRate, c(0.05, 0.5, 0.95))
quantile_birth_seq = quantile(dat_seq$birthRate, c(0.05, 0.5, 0.95))

width_birth_noseq = quantile_birth_noseq[3] - quantile_birth_noseq[1]
width_birth_seq = quantile_birth_seq[3] - quantile_birth_seq[1]
width_birth_seq
width_birth_noseq

quantile_death_noseq = quantile(dat_no_seq$deathRate, c(0.05, 0.5, 0.95))
quantile_death_seq = quantile(dat_seq$deathRate, c(0.05, 0.5, 0.95))

width_death_noseq = quantile_death_noseq[3] - quantile_death_noseq[1]
width_death_seq = quantile_death_seq[3] - quantile_death_seq[1]
width_death_seq
width_death_noseq
