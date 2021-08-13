Djed Stablecoin
====================================================================================================================================================================================
A prototype implementation of Djed stablecoin presented in the paper [Djed: A Formally Verified Crypto-Backed Pegged Algorithmic Stablecoin](https://eprint.iacr.org/????).
The prototype implements both Minimal and Extended versions of Djed.

The repository also contains an implementation of a simple ledger and simulator that can be used to perform various economic simulations.  

Minimal Djed
-------------------
Minimal Djed implementation has several divergences with the model described in the paper:
- Reservecoins buying and selling utilize formulas with continuous price recalculation (in the paper this feature is introduced only for Extended Djed). Though stablecoins buying and selling are done with fixed price as originally described in the paper.
- There is no implementation of the initialization period when reservecoins buying is not restricted by maximal reserves ratio.

Extended Djed
-------------------
Extended Djed is based on Minimal Djed and implements all additional features described in the paper:
- dynamically adjusted fees depending on the deviation from the optimal reserves ratio;
- there are no minimal and maximal reserves ratio anymore, operations are constrained by dynamic fees;
- continuous price recalculation is used to define the price for all operations;
- additional measures to support reservecoins price when reserve ratio falls significanly;
- debt-for-equity swaps to compensate stablecoin holders when the peg is lost.

Extended Djed implementation also contains a special set of unit tests to cross-check formulas in the continuous setting against their discrete versions providing assurance that the formulas presented in the paper were derived correctly.

Current status
-------------------
Current implementation is a prototype which may have some divergences with the paper. Its main purpose is to test the model, formulas and perform economic simulations.

Other applications
-------------------
It might be useful for developers as a reference implementation, especially to understand complex formulas in Extended Djed.    
 


