{-# LANGUAGE OverloadedStrings #-}

module UseCases.ReadLottery where

import Models.Lottery

getLottery :: Lottery
getLottery = new "Test lottery"

buyTicket :: TicketHolder -> Lottery
buyTicket = add getLottery
