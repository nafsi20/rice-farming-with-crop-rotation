# Seminar 6: Model programming

library(decisionSupport)
library(DiagrammeR)

mermaid("graph LR
        CR(Crop rotation)-->RY(Rice yield); linkStyle 0 stroke:purple, stroke-width:1.5px
        CR(Crop rotation)-->SY(Soybean yield); linkStyle 0 stroke:purple, stroke-width:1.5px
        CR(Crop rotation)-->CY(Chilli yield); linkStyle 0 stroke:purple, stroke-width:1.5px
        CR-->LC(Labor); linkStyle 0 stroke:red, stroke-width:1.5px
        CR-->SC(Seed); linkStyle 0 stroke:red, stroke-width:1.5px
        CR-->FC(Fertilizer); linkStyle 0 stroke:red, stroke-width:1.5px
        CR-->PC(Pesticides); linkStyle 0 stroke:red, stroke-width:1.5px
        CR-->HC(Harvesting); linkStyle 0 stroke:red, stroke-width:1.5px
        CR-->MC(Machinery); linkStyle 0 stroke:red, stroke-width:1.5px
        CR-->RC(Rent land); linkStyle 0 stroke:red, stroke-width:1.5px
        RY-->R(Revenue);linkStyle 0 stroke:green, stroke-width:2px
        SY-->R;linkStyle 0 stroke:green, stroke-width:2px
        CY-->R;linkStyle 0 stroke:green, stroke-width:2px
        LC-->TC(Total costs);linkStyle 0 stroke:red, stroke-width:2px
        SC-->TC;linkStyle 0 stroke:red, stroke-width:2px
        FC-->TC;linkStyle 0 stroke:red, stroke-width:2px
        PC-->TC;linkStyle 0 stroke:red, stroke-width:2px
        HC-->TC;linkStyle 0 stroke:red, stroke-width:2px
        RC-->TC;linkStyle 0 stroke:red, stroke-width:2px
        R-->NPV(Net present value); linkStyle 0 stroke:green, stroke-width:3px
        TC-->NPV;linkStyle 0 stroke:red, stroke-width:3px")
        

