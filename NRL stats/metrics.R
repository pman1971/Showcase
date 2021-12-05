metric= c('Points',
          'Tries',
          'Runs',
          'RunMetres',
          'Linebreaks',
          'TackleBusts',
          'Offloads',
          'TackledOpp20',
          'CompletionRate',
          'Goals',
          'GoalAttempts',
          'GoalKicking%',
          'FieldGoals',
          'FieldGoalAttempts',
          'Kicks',
          'KicMetres',
          'LongKicks',
          'WeightedKicks',
          'AttackingKicks',
          'ForcedDropouts',
          'KickReturns',
          'KickReturnMetres',
          'DummyHalfRuns',
          'OnePassHitUps',
          'LineEngagements',
          'GeneralPlayPasses',
          'InGoalEscapes',
          'Errors',
          'PenaltiesAwarded',
          'PenaltieConceded',
          'SinBins',
          'SendOffs',
          'PointConceded',
          'TriesConceded',
          'RunMetresConceded',
          'LinebreaksConceded',
          'Tackles',
          'MissedTackles',
          'OffloadsConceded',
          'TryAssists')

statLegend= names(mydata)

# ATTACK
metricAttack= c('PTS',
                'T',
                'R',
                'RM',
                'LB',
                'TB',
                'OFF',
                'TO20',
                'CR%',
                'KR',
                'KRM',
                'DHR',
                '1PH',
                'LE',
                'GPP',
                'IGE',
                'TA')


attackLegend= data.frame(statLegend= metricAttack,
                         statType= 'Attack')

# KICKING
metricKick= c('GLS',
              'GA',
              'GK%',
              'FG',
              'FG',
              'K',
              'KM',
              'LK',
              'WK',
              'AK',
              'FDO')

kickLegend= data.frame(statLegend= metricKick,
                       statType= 'Kick')

# DEFENSE
metricDefense= c('ERR',
                 'PA',
                 'PC',
                 'SB',
                 'SO',
                 'PTSC',
                 'TC',
                 'RMC',
                 'LBC',
                 'TCK',
                 'MT',
                 'OFFC')

defenseLegend= data.frame(statLegend= metricDefense,
                          statType= 'Defense&Errors')

completeLegend= rbind(attackLegend, kickLegend, defenseLegend)


# Check names
setdiff(metricAttack, statLegend)
setdiff(metricKick, statLegend)
setdiff(metricDefense, statLegend)

legendDF= data.frame(statLegend,
                     metric)

legendDF=
  legendDF %>%
  inner_join(completeLegend, by= c('statLegend'))

legendDF$legendCol= ifelse(legendDF$statType== 'Attack', 'Green',
                           ifelse(legendDF$statType== 'Kick', 'Blue', 'Red'))


playerLegend= c('G',
                'Games',
                'ST',
                'Starts',
                'MIN',
                'Minutes',
                'T',
                'Tries',
                'TA',
                'TryAssists',
                'GLS',
                'Goals',
                'GK%',
                'GoalKicking%',
                'FG',
                'FieldGoals',
                'PTS',
                'Points',
                'R',
                'Runs',
                'RM',
                'RunMetres',
                'LB',
                'Linebreaks',
                'TB',
                'TackleBusts',
                'OFF',
                'Offloads',
                'K',
                'Kicks',
                'KM',
                'KickMetres',
                'TCK',
                'Tackles',
                'MT',
                'MissedTackles',
                'ERR',
                'Errors',
                'PEN',
                'Penalties',
                'SB',
                'SinBins',
                'SO',
                'SendOffs'
)
